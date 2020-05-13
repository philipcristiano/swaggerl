-module(swaggerl).
-include_lib("kernel/include/logger.hrl").

-export([load/1,
         load/2,
         op/3,
         op/4,
         async_op/3,
         async_op/4,
         operations/1,
         set_server/2
         ]).

-export_type([swaggerl_api/0]).

-record(state, {spec,
                ops_map,
                server,
                httpoptions=[],
                swaggerl_options=[],
                path
}).

%%% API

-opaque swaggerl_api() :: #state{}.


-spec load(list()) -> swaggerl_api().
load(Path) when is_list(Path)->
    load(Path, []).

-spec load(list(), list()) -> swaggerl_api().
load(Path, Options) when is_list(Path) and is_list(Options)->
    {SwaggerlOptions, HTTPOptions} = split_options(Options),
    Data = case Path of
        [$h, $t, $t, $p | _Rest] = Path -> load_http(Path, HTTPOptions);
        _ -> load_file(Path)
    end,
    ?LOG_DEBUG(#{msg=>"Loaded config"}),
    decode_data(Data,
                #state{httpoptions=HTTPOptions,
                swaggerl_options=SwaggerlOptions}).

-spec op(swaggerl_api(), binary() | list(), list()) -> any().
op(S=#state{}, Op, Params) when is_list(Op)->
    BOp = binary:list_to_bin(Op),
    op(S, BOp, Params, []);
op(S=#state{}, Op, Params) ->
    op(S, Op, Params, []).

op(S=#state{}, Op, Params, ExtraOps) when is_list(Op)->
    BOp = binary:list_to_bin(Op),
    op(S, BOp, Params, ExtraOps);
op(S=#state{}, Op, Params, ExtraOps) ->
    try op_to_request(S, Op, Params, ExtraOps, []) of
        {ok, _Code, _Headers, ReqRef} ->
            {ok, Body} = hackney:body(ReqRef),
            Data = jsx:decode(Body, [return_maps]),
            Data
    catch
        Throw -> Throw
    end.

-spec async_op(swaggerl_api(), binary() | list(), list()) -> any().
async_op(S=#state{}, Op, Params) when is_list(Op)->
    BOp = binary:list_to_bin(Op),
    async_op(S, BOp, Params, []);
async_op(S=#state{}, Op, Params) ->
    async_op(S, Op, Params, []).

async_op(S=#state{}, Op, Params, ExtraOps) ->
    AsyncOps = [{recv_timeout, infinity}, async],
    try op_to_request(S, Op, Params, ExtraOps, AsyncOps) of
          {ok, RequestId} -> Callback = fun(Msg) ->
                                async_read(S, RequestId, Msg) end,
                             Callback
    catch
        Throw -> Throw
    end.

op_to_request(#state{ops_map=OpsMap, server=Server, httpoptions=HTTPOptions},
            Op, Params, ExtraOps, InternalHTTPOps) ->
    {Method, Path, PayloadHeaders, Payload} = request_details(
        Server, Op, OpsMap, Params, ExtraOps),
    Headers = proplists:get_value(default_headers,
                                  HTTPOptions,
                                  []),
    RequestHeaders = Headers ++ PayloadHeaders,
    NonSwaggerlHTTPOptions = proplists:delete(default_headers,
                                              HTTPOptions),
    ExtraHTTPOps = proplists:get_value(http_options, ExtraOps, []),
    AllHTTPOps = NonSwaggerlHTTPOptions ++ ExtraHTTPOps ++ InternalHTTPOps,
    hackney:request(Method, Path, RequestHeaders, Payload, AllHTTPOps).


-spec operations(swaggerl:swaggerl_api()) -> [[byte()]].
operations(#state{ops_map=OpsMap}) ->
    Keys = maps:keys(OpsMap),
    ListKeys = list_of_bins_to_list_of_lists(Keys),
    ListKeys.

-spec set_server(swaggerl:swaggerl_api(), list()) -> swaggerl:swaggerl_api().
set_server(State=#state{}, Server) ->
    State#state{server=Server}.

%%% Internal

split_options(Options) ->
    ReducedOperations = proplists:get_value(operations, Options, all_ops),
    HTTPOptions = proplists:delete(operations, Options),
    {[{operations, ReducedOperations}], HTTPOptions}.


request_details(Server, Op, OpsMap, InParams, ExtraOps) ->
    {Path, Method, OpSpec} = maps:get(Op, OpsMap),
    Params = normalize_param_names(InParams),
    % TODO: Need to test for lack of parameters in the op
    ParamSpecs = maps:get(<<"parameters">>, OpSpec, []),
    SortedParams = sort_params(ParamSpecs, Params, #{}),
    PathParams = maps:get(path, SortedParams, []),
    ReplacedPath = binary:bin_to_list(
        replace_path(Path, PathParams)),
    FullPath = Server ++ ReplacedPath,

    QueryParams = maps:get(query, SortedParams, []),
    PathWithQueryParams = add_query_params(
        FullPath, QueryParams),

    BodyParam = maps:get(body, SortedParams, []),
    BodyHeader = proplists:get_value(
        content_type, ExtraOps, <<"application/json">>),
    {Headers, Payload} = encode_body(BodyParam, BodyHeader, fun jsx:encode/1),

    AMethod = method(Method),
    {AMethod, PathWithQueryParams, Headers, Payload}.

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

-spec encode_body(list(), binary(), fun((_) -> binary())) -> {list(), binary()}.
encode_body([], _ContentTypeHeader, _EncodeFun) ->
    {[], <<>>};
encode_body([{_Key, Body}], ContentTypeHeader, EncodeFun) ->
    Headers = [{<<"content-type">>, ContentTypeHeader}],
    Payload = EncodeFun(Body),
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
    throw({error, missing_required_field, Name});
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

-spec decode_data(any(), swaggerl_api()) -> swaggerl_api().
decode_data(Data, State=#state{swaggerl_options=SwaggerlOptions}) ->
    Spec = jsx:decode(Data, [return_maps]),
    Operations = proplists:get_value(operations, SwaggerlOptions),
    OpsMap = create_ops_map(Spec, Operations),
    State#state{ops_map=OpsMap}.

create_ops_map(Spec, Operations) ->
    OpsMap0 = maps:new(),
    Paths = maps:get(<<"paths">>, Spec, #{}),
    {_, _, _, OpsMap1} = maps:fold(fun add_paths_to_ops_map/3,
                                   {Operations, OpsMap0},
                                   Paths),
    OpsMap1.

add_paths_to_ops_map(Path, Data,
                     {_PreviousPath, _PreviousPathProps, Operations, OpsMap}) ->
    add_paths_to_ops_map(Path, Data, {Operations, OpsMap});
add_paths_to_ops_map(Path, Data, {Operations, OpsMap}) ->
    PathItemParams = maps:get(<<"parameters">>, Data, []),
    PathProperties = #{parameters => PathItemParams},
    maps:fold(fun add_path_op_to_ops_map/3,
              {Path, PathProperties, Operations, OpsMap},
              Data).

add_path_op_to_ops_map(Method, [Data],
                       {Path, PathProperties, Operations, OpsMap}) ->
    add_path_op_to_ops_map(Method, Data,
                          {Path, PathProperties, Operations, OpsMap});
add_path_op_to_ops_map(Method, Data,
                       {Path, PathProperties, Operations, OpsMap}
                       ) when is_map(Data)->
    PathItemParams = maps:get(parameters, PathProperties),
    case maps:is_key(<<"operationId">>, Data) of
        false -> {Path, PathProperties, Operations, OpsMap};
        true  -> Op = maps:get(<<"operationId">>, Data),
                 case include_op(Op, Operations) of
                   false -> {Path, PathProperties, Operations, OpsMap};
                   true ->
                     % Combine path item params and the op params
                     OpParams = maps:get(<<"parameters">>, Data, []),
                     NewData = maps:put(<<"parameters">>,
                                        OpParams ++ PathItemParams,
                                        Data),
                     NewOpsMap = maps:put(Op, {Path, Method, NewData}, OpsMap),

                     {Path, PathProperties, Operations, NewOpsMap}
                end
    end;
add_path_op_to_ops_map(_Method, _Data,
                       {Path, PathProperties, Operations, OpsMap}) ->
    {Path, PathProperties, Operations, OpsMap}.

% Filter out the right operations
include_op(_Op, all_ops) ->
    true;
include_op(Op, Operations) ->
    lists:member(Op, Operations).

method(<<"get">>) ->
    get;
method(<<"patch">>) ->
    patch;
method(<<"post">>) ->
    post;
method(<<"put">>) ->
    put.

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
    decode_multi_json_docs(Bin);
async_read(_S, _Ref, _Unknown) ->
    unknown.


decode_multi_json_docs(<<>>) ->
    [];
decode_multi_json_docs(Bin) ->
    {with_tail, Term, Rest} = jsx:decode(Bin, [return_maps, return_tail]),
    [Term | decode_multi_json_docs(Rest)].

normalize_param_names([{Name, Value} | T]) when is_list(Name) ->
    BName = binary:list_to_bin(Name),
    [{BName, Value}] ++ normalize_param_names(T);
normalize_param_names([{Name, Value} | T]) ->
    [{Name, Value}] ++ normalize_param_names(T);
normalize_param_names([]) ->
    [].
