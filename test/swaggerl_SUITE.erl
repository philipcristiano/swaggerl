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
              bb_get_operation_with_http_options
              ]}].



init_per_testcase(_, Config) ->
    ok = lager_common_test_backend:bounce(debug),
    ok = meck:new(httpc, []),
    DataDir = ?config(data_dir, Config),
    PetSwagger = DataDir ++ "swagger-pets.json",

    [{pet_swagger, PetSwagger} | Config].

end_per_testcase(_, Config) ->
    meck:unload(httpc),
    Config.

aa_load_test(Config) ->
    PetSwagger = ?config(pet_swagger, Config),
    Conf = ?MUT:load(PetSwagger),
    Conf.

ba_simple_get_operation(Config) ->
    Conf0 = load_pet_fixture(Config),
    Result = {"Status", "Headers", jsx:encode(#{})},
    ok = meck:expect(httpc, request, fun(get, {"http://localhost/pet/0", []}, [], [{body_format, binary}]) -> {ok, Result} end),

    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Resp = ?MUT:op(Conf1, "getPetById", [{"petId", 0}]),
    true = meck:validate(httpc),
    ?assertEqual(#{}, Resp),
    ok.

bb_get_operation_with_http_options(Config) ->
    HTTPOptions = make_ref(),
    Conf0 = load_pet_fixture(Config, HTTPOptions),
    Result = {"Status", "Headers", jsx:encode(#{})},
    ok = meck:expect(httpc, request, fun(get,
                                         {"http://localhost/pet/0", []},
                                        FunHTTPOptions,
                                        [{body_format, binary}]) ->
        ?assertEqual(HTTPOptions, FunHTTPOptions),
        {ok, Result} end),

    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Resp = ?MUT:op(Conf1, "getPetById", [{"petId", 0}]),
    true = meck:validate(httpc),
    ?assertEqual(#{}, Resp),
    ok.


load_pet_fixture(Config) ->
    load_pet_fixture(Config, []).

load_pet_fixture(Config, Options) ->
    PetSwagger = ?config(pet_swagger, Config),
    ?MUT:load(PetSwagger, Options).
