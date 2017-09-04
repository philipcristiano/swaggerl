-module(swaggerl_SUITE).

% -compile({parse_transform, lager_transform}).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").


-define(MUT, swaggerl).
-define(File, "priv/swagger-pets.json").


all() -> [{group, test_swaggerl}].

groups() -> [{test_swaggerl,
             [aa_load_test,
              ba_simple_get_operation,
              bb_get_operation_with_http_options,
              ca_list_operations,
              da_async_get_operation
              ]}].



init_per_testcase(_, Config) ->
    ok = lager_common_test_backend:bounce(debug),
    ok = meck:new(hackney, []),
    DataDir = ?config(data_dir, Config),
    PetSwagger = DataDir ++ "swagger-pets.json",
    ok = meck:expect(hackney, body, fun(PL) -> proplists:get_value(body, PL) end),

    [{pet_swagger, PetSwagger} | Config].

end_per_testcase(_, Config) ->
    meck:unload(hackney),
    Config.

aa_load_test(Config) ->
    PetSwagger = ?config(pet_swagger, Config),
    Conf = ?MUT:load(PetSwagger),
    Conf.

ba_simple_get_operation(Config) ->
    Conf0 = load_pet_fixture(Config),
    Result = hackney_response([{body, {ok, jsx:encode(#{})}}]),
    ok = meck:expect(hackney, request, fun(get, "http://localhost/pet/0", [], <<>>, []) -> Result end),

    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Resp = ?MUT:op(Conf1, "getPetById", [{"petId", 0}]),
    true = meck:validate(hackney),
    ?assertEqual(#{}, Resp),
    ok.

bb_get_operation_with_http_options(Config) ->
    HTTPOptions = make_ref(),
    Conf0 = load_pet_fixture(Config, HTTPOptions),
    Result = hackney_response([{body, {ok, jsx:encode(#{})}}]),
    ok = meck:expect(hackney, request, fun(get,
                                           "http://localhost/pet/0",
                                           [],
                                           <<>>,
                                           FunHTTPOptions) ->
        ?assertEqual(HTTPOptions, FunHTTPOptions),
        Result end),

    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Resp = ?MUT:op(Conf1, "getPetById", [{"petId", 0}]),
    true = meck:validate(hackney),
    ?assertEqual(#{}, Resp),
    ok.

ca_list_operations(Config) ->
    Conf = load_pet_fixture(Config),
    Resp = ?MUT:operations(Conf),
    ?assertEqual(pet_operations(), Resp),
    ok.

da_async_get_operation(Config) ->
    Conf0 = load_pet_fixture(Config),
    Ref = make_ref(),
    Result = {hackney_response, Ref, jsx:encode(#{})},
    ok = meck:expect(hackney, request, fun(get,
                                           Path,
                                           Headers,
                                           <<>>,
                                           Options) ->
        ?assertEqual("http://localhost/pet/0", Path),
        ?assertEqual([], Headers),
        ?assertEqual([{recv_timeout,infinity}, async], Options),
        async_http_send(self(), Result),
        {ok, Ref} end),

    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Callback = ?MUT:async_op(Conf1, "getPetById", [{"petId", 0}]),
    Msg = get_msg(),
    Resp = Callback(Msg),
    true = meck:validate(hackney),
    ?assertEqual(#{}, Resp),
    ok.

hackney_response(Result) ->
    {ok, code, headers, Result}.

load_pet_fixture(Config) ->
    load_pet_fixture(Config, []).

load_pet_fixture(Config, Options) ->
    PetSwagger = ?config(pet_swagger, Config),
    ?MUT:load(PetSwagger, Options).

async_http_send(Pid, Body) ->
    Pid ! Body.

pet_operations() ->
    ["addPet",
     "createUser",
     "createUsersWithArrayInput",
     "createUsersWithListInput",
     "deleteOrder",
     "deletePet",
     "deleteUser",
     "findPetsByStatus",
     "findPetsByTags",
     "getInventory",
     "getOrderById",
     "getPetById",
     "getUserByName",
     "loginUser",
     "logoutUser",
     "placeOrder",
     "updatePet",
     "updatePetWithForm",
     "updateUser",
     "uploadFile"
    ].

get_msg() ->
    receive
        Msg -> Msg
    after 1000 ->
        error
    end.
