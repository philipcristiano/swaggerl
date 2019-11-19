-module(swaggerl).
-include_lib("kernel/include/logger.hrl").

-export([load/1,
         load/2,
         op/3,
         op/4,
         async_op/3,
         operations/1,
         set_server/2
         ]).

-export_type([swaggerl_api/0]).

-record(state, {spec,
                ops_map,
                server,
                httpoptions=[],
                path
}).

%%% API

-opaque swaggerl_api() :: #state{spec::map()}.


-spec load(list()) -> swaggerl_api().
load(Path) when is_list(Path)->
    load(Path, []).

-spec load(list(), list()) -> swaggerl_api().
load(Path, HTTPOptions) when is_list(Path) and is_list(HTTPOptions)->
    Data = case Path of
        [$h, $t, $t, $p | _Rest] = Path -> load_http(Path, HTTPOptions);
        _ -> load_file(Path)
    end,
    ?LOG_DEBUG(#{msg=>"Loaded config"}),
    decode_data(Data, #state{httpoptions=HTTPOptions}).

-spec op(swaggerl_api(), binary() | list(), list()) -> any().
op(S=#state{}, Op, Params) when is_list(Op)->
    BOp = binary:list_to_bin(Op),
    op(S, BOp, Params, []);
op(S=#state{}, Op, Params) ->
    op(S, Op, Params, []).

op(S=#state{}, Op, Params, ExtraHTTPOps) when is_list(Op)->
    BOp = binary:list_to_bin(Op),
    op(S, BOp, Params, ExtraHTTPOps);
op(#state{ops_map=OpsMap, server=Server, httpoptions=HTTPOptions},
        Op, Params, ExtraHTTPOps) ->
    RequestDetails = request_details(Server, Op, OpsMap, Params),
    case RequestDetails of
        {error, Reason, Info} -> {error, Reason, Info};
        {Method, Path, PayloadHeaders, Payload} ->
            Headers = proplists:get_value(default_headers,
                                          HTTPOptions,
                                          []),
            RequestHeaders = Headers ++ PayloadHeaders,
            NonSwaggerlHTTPOptions = proplists:delete(default_headers,
                                                      HTTPOptions),
            CombinedHTTPOptions = NonSwaggerlHTTPOptions ++ ExtraHTTPOps,
            {ok, _Code, _Headers, ReqRef} = hackney:request(
                Method, Path, RequestHeaders, Payload, CombinedHTTPOptions),
            {ok, Body} = hackney:body(ReqRef),
            Data = jsx:decode(Body, [return_maps]),
            Data
    end.

-spec async_op(swaggerl_api(), binary() | list(), list()) -> any().
async_op(S=#state{}, Op, Params) when is_list(Op)->
    BOp = binary:list_to_bin(Op),
    async_op(S, BOp, Params);
async_op(S=#state{ops_map=OpsMap, server=Server, httpoptions=HTTPOptions},
            Op, Params) ->
    RequestDetails = request_details(Server, Op, OpsMap, Params),
    case RequestDetails of
        {error, Reason, Info} -> {error, Reason, Info};
        {Method, Path, PayloadHeaders, Payload} ->
          Headers = proplists:get_value(default_headers, HTTPOptions, []),
          RequestHeaders = Headers ++ PayloadHeaders,
          NonSwaggerlHTTPOptions = proplists:delete(
              default_headers, HTTPOptions),
          Options = [{recv_timeout, infinity},
                      async] ++ NonSwaggerlHTTPOptions,
          {ok, RequestId} = hackney:request(
              Method, Path, RequestHeaders, Payload, Options),
          Callback = fun(Msg) ->
              async_read(S, RequestId, Msg) end,

          Callback
    end.

-spec operations(swaggerl:swaggerl_api()) -> [[byte()]].
operations(#state{ops_map=OpsMap}) ->
    Keys = maps:keys(OpsMap),
    ListKeys = list_of_bins_to_list_of_lists(Keys),
    ListKeys.

-spec set_server(swaggerl:swaggerl_api(), list()) -> swaggerl:swaggerl_api().
set_server(State=#state{}, Server) ->
    State#state{server=Server}.

%%% Internal

request_details(Server, Op, OpsMap, InParams) ->
    {Path, Method, OpSpec} = maps:get(Op, OpsMap),
    Params = normalize_param_names(InParams),
    % TODO: Need to test for lack of parameters in the op
    ParamSpecs = maps:get(<<"parameters">>, OpSpec, []),
    SortedParams = sort_params(ParamSpecs, Params, #{}),

    case SortedParams of
        {error, Reason, Info} -> {error, Reason, Info};
        SortedParams -> PathParams = maps:get(path, SortedParams, []),
                        ReplacedPath = binary:bin_to_list(
                            replace_path(Path, PathParams)),
                        FullPath = Server ++ ReplacedPath,

                        QueryParams = maps:get(query, SortedParams, []),
                        PathWithQueryParams = add_query_params(
                            FullPath, QueryParams),

                        BodyParam = maps:get(body, SortedParams, []),
                        {Headers, Payload} = encode_body(BodyParam),

                        AMethod = method(Method),
                        {AMethod, PathWithQueryParams, Headers, Payload}
    end.

sort_params([], _Params, Sorted) ->
    Sorted;
sort_params([H|T], Params, Sorted) ->
    In = in_type(maps:get(<<"in">>, H)),
    Name = maps:get(<<"name">>, H),
    Required = maps:get(<<"required">>, H, false),

    Value = proplists:get_value(Name, Params),
    Sort = maps:get(In, Sorted, []),
    NewSort = add_sort_param_to_proplist(Required, Name, Value, Sort),
    case NewSort of
        {error, Reason, Info} -> {error, Reason, Info};
        NewSort -> NewSorted = maps:put(In, NewSort, Sorted),
                   sort_params(T, Params, NewSorted)
    end.

-spec encode_body(list()) -> {list(), binary()}.
encode_body([]) ->
    {[], <<>>};
encode_body([{_Key, Body}]) ->
    Headers = [{<<"content-type">>, <<"application/json">>}],
    Payload = jsx:encode(Body),
    {Headers, Payload}.

-spec in_type(binary()) -> atom().
in_type(<<"path">>) ->
    path;
in_type(<<"query">>) ->
    query;
in_type(<<"body">>) ->
    body.


add_sort_param_to_proplist(false, _, undefined, Sort) ->
    Sort;
add_sort_param_to_proplist(true, Name, undefined, _Sort) ->
    {error, missing_required_field, Name};
add_sort_param_to_proplist(_, Name, Value, Sort) ->
    Sort ++ [{Name, Value}].


add_query_params(Path, []) ->
    Path;
add_query_params(Path, Params) ->
    QueryParams = uri_string:compose_query(Params),
    Path ++ "?" ++ QueryParams.

load_file(Path) ->
    {ok, Data} = file:read_file(Path),
    Data.

load_http(Path, HTTPOptions) ->

    ?LOG_DEBUG(#{msg=>"Loading HTTP Config"}),
    Headers = proplists:get_value(default_headers, HTTPOptions, []),
    NonSwaggerlHTTPOptions = proplists:delete(default_headers, HTTPOptions),

    Resp = hackney:request(get, Path, Headers, <<>>, NonSwaggerlHTTPOptions),
    ReturnBody = case Resp of
        {ok, _Code, _Headers, ReqRef} -> {ok, Body} = hackney:body(ReqRef),
                                         Body;
        Else -> ?LOG_ERROR(#{msg=>"Error requesting swagger config",
                             error=>Else}),
                {error, Else}
    end,
    ReturnBody.

decode_data(Data, State=#state{}) ->
    Spec = jsx:decode(Data, [return_maps]),
    OpsMap = create_ops_map(Spec),
    State#state{spec=Spec, ops_map=OpsMap}.

create_ops_map(Spec) ->
    OpsMap0 = maps:new(),
    Paths = maps:get(<<"paths">>, Spec, #{}),
    {_, _, OpsMap1} = maps:fold(fun add_paths_to_ops_map/3, OpsMap0, Paths),
    OpsMap1.

add_paths_to_ops_map(Path, Data, {_PreviousPath, _PreviousPathProps, OpsMap}) ->
    add_paths_to_ops_map(Path, Data, OpsMap);
add_paths_to_ops_map(Path, Data, OpsMap) ->
    PathItemParams = maps:get(<<"parameters">>, Data, []),
    PathProperties = #{parameters => PathItemParams},
    maps:fold(fun add_path_op_to_ops_map/3,
              {Path, PathProperties, OpsMap},
              Data).

add_path_op_to_ops_map(Method, [Data], {Path, PathProperties, OpsMap}) ->
    add_path_op_to_ops_map(Method, Data, {Path, PathProperties, OpsMap});
add_path_op_to_ops_map(Method, Data,
                       {Path, PathProperties, OpsMap}) when is_map(Data)->
    PathItemParams = maps:get(parameters, PathProperties),
    case maps:is_key(<<"operationId">>, Data) of
        false -> {Path, PathProperties, OpsMap};
        true  -> Op = maps:get(<<"operationId">>, Data),

                 % Combine path item params and the op params
                 OpParams = maps:get(<<"parameters">>, Data, []),
                 NewData = maps:put(<<"parameters">>,
                                    OpParams ++ PathItemParams,
                                    Data),
                 NewOpsMap = maps:put(Op, {Path, Method, NewData}, OpsMap),

                 {Path, PathProperties, NewOpsMap}
    end;
add_path_op_to_ops_map(_Method, _Data, {Path, PathProperties, OpsMap}) ->
    {Path, PathProperties, OpsMap}.

method(<<"get">>) ->
    get;
method(<<"post">>) ->
    post.

replace_path(Path, []) ->
    Path;
replace_path(Path, [{ParamK, ParamV}|Params]) when is_list(ParamK) ->
    BParamK = binary:list_to_bin(ParamK),
    replace_path(Path, [{BParamK, ParamV}|Params]);
replace_path(Path, [{ParamK, ParamV}|Params]) when is_list(ParamV) ->
    BParamV = binary:list_to_bin(ParamV),
    replace_path(Path, [{ParamK, BParamV}|Params]);
replace_path(Path, [{ParamK, ParamV}|Params]) when is_integer(ParamV) ->
    BParamV = binary:list_to_bin(integer_to_list(ParamV)),
    replace_path(Path, [{ParamK, BParamV}|Params]);
replace_path(Path, [{ParamK, ParamV}|Params]) when is_binary(ParamK) ->
    ReplaceBin = << <<"{">>/binary, ParamK/binary, <<"}">>/binary >>,
    NewPath = binary:replace(Path, ReplaceBin, ParamV),
    replace_path(NewPath, Params).

list_of_bins_to_list_of_lists([]) ->
    [];
list_of_bins_to_list_of_lists([H|T]) ->
    [binary:bin_to_list(H) | list_of_bins_to_list_of_lists(T)].

async_read(_S=#state{}, Ref,
      {hackney_response, Ref, {status, StatusInt, _Reason}}) ->
    {status, StatusInt};
async_read(_S=#state{}, Ref, {hackney_response, Ref, {headers, _Headers}}) ->
    ok;
async_read(_S=#state{}, Ref, {hackney_response, Ref, done}) ->
    done;
async_read(_S=#state{}, Ref, {hackney_response, Ref, Bin}) ->
    jsx:decode(Bin, [return_maps]);
async_read(_S, _Ref, _Unknown) ->
    unknown.

normalize_param_names([{Name, Value} | T]) when is_list(Name) ->
    BName = binary:list_to_bin(Name),
    [{BName, Value}] ++ normalize_param_names(T);
normalize_param_names([{Name, Value} | T]) ->
    [{Name, Value}] ++ normalize_param_names(T);
normalize_param_names([]) ->
    [].
