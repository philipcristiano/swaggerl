-module(swaggerl).

-compile({parse_transform, lager_transform}).

-export([load/1,
         load/2,
         op/3,
         operations/1,
         set_server/2
         ]).

-record(state, {spec,
                ops_map,
                server,
                httpoptions=[],
                path
}).

%%% API

load(Path) ->
    load(Path, []).

load(Path, HTTPOptions) ->
    Data = case Path of
        [$h, $t, $t, $p | _Rest] = Path -> load_http(Path, HTTPOptions);
        _ -> load_file(Path)
    end,
    lager:debug("HTTPOptions ~p", [HTTPOptions]),
    decode_data(Data, #state{httpoptions=HTTPOptions}).

op(S=#state{}, Op, Params) when is_list(Op)->
    BOp = binary:list_to_bin(Op),
    op(S, BOp, Params);
op(#state{ops_map=OpsMap, server=Server, httpoptions=HTTPOptions}, Op, Params) ->
    {Path, Method, _OpSpec} = maps:get(Op, OpsMap),
    ReplacedPath = binary:bin_to_list(replace_path(Path, Params)),
    FullPath = Server ++ ReplacedPath,
    AMethod = method(Method),
    lager:debug("Found op ~p", [Op]),
    {ok, Result} = httpc:request(AMethod, {FullPath, []}, HTTPOptions, [{body_format, binary}]),
    {_Status, _Headers, Body} = Result,
    lager:debug("Found Body ~p", [Body]),
    Data = jsx:decode(Body, [return_maps]),
    lager:debug("Found Data ~p", [Data]),
    Data.

operations(#state{ops_map=OpsMap}) ->
    Keys = maps:keys(OpsMap),
    ListKeys = list_of_bins_to_list_of_lists(Keys),
    ListKeys.

set_server(State=#state{}, Server) ->
    State#state{server=Server}.

%%% Internal

load_file(Path) ->
    {ok, Data} = file:read_file(Path),
    Data.

load_http(Path, HTTPOptions) ->
    io:format("Options ~p~n", [HTTPOptions]),
    {ok, Result} = httpc:request(get, {Path, []}, HTTPOptions, [{body_format, binary}]),
    {_Status, _Headers, Body} = Result,
    Body.

decode_data(Data, State=#state{}) ->
    Spec = jsx:decode(Data, [return_maps]),
    OpsMap = create_ops_map(Spec),
    State#state{spec=Spec, ops_map=OpsMap}.

create_ops_map(Spec) ->
    OpsMap0 = maps:new(),
    Paths = maps:get(<<"paths">>, Spec, #{}),
    {_, OpsMap1} = maps:fold(fun add_paths_to_ops_map/3, OpsMap0, Paths),
    OpsMap1.

add_paths_to_ops_map(Path, Data, {_PreviousPath, OpsMap}) ->
    add_paths_to_ops_map(Path, Data, OpsMap);
add_paths_to_ops_map(Path, Data, OpsMap) ->
    lager:debug("Add paths to ops map ~p", [Path]),
    maps:fold(fun add_path_op_to_ops_map/3, {Path, OpsMap}, Data).

add_path_op_to_ops_map(Method, [Data], {Path, OpsMap}) ->
    add_path_op_to_ops_map(Method, Data, {Path, OpsMap});
add_path_op_to_ops_map(Method, Data, {Path, OpsMap}) when is_map(Data)->
    lager:debug("Get Operation ID from ~p", [Data]),
    case maps:is_key(<<"operationId">>, Data) of
        false -> {Path, OpsMap};
        true  -> Op = maps:get(<<"operationId">>, Data),
                 NewOpsMap = maps:put(Op, {Path, Method, Data}, OpsMap),
                 {Path, NewOpsMap}
    end;
add_path_op_to_ops_map(_Method, Data, {Path, OpsMap}) ->
    lager:debug("Not a map :/ ~n", [Data]),
    {Path, OpsMap}.

method(<<"get">>) ->
    get.

replace_path(Path, []) ->
    Path;
replace_path(Path, [{ParamK, ParamV}|Params]) when is_list(ParamK) ->
    BParamK = binary:list_to_bin(ParamK),
    replace_path(Path, [{BParamK, ParamV}|Params]);
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
