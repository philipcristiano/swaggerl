-module(swaggerl_SUITE).

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
              bc_list_operation_with_query_string,
              ca_list_operations,
              da_async_get_operation,
              ea_load_with_http_headers,
              eb_get_with_http_headers,
              ec_async_get_with_http_headers,
              fa_get_without_required_param,
              fb_async_get_without_required_param,
              ga_post_operation
              ]}].



init_per_testcase(_, Config) ->
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

    ok = meck:expect(hackney, request, fun(get,
                                           URL,
                                           RequestHeaders,
                                           Body,
                                           _FunHTTPOptions) ->
        ?assertEqual(URL, "http://localhost/pets/0"),
        ?assertEqual([], RequestHeaders),
        ?assertEqual(<<>>, Body),
        Result
    end),
    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Resp = ?MUT:op(Conf1, <<"find pet by id">>, [{"id", "0"}]),
    true = meck:validate(hackney),
    ?assertEqual(#{}, Resp),
    ok.

bb_get_operation_with_http_options(Config) ->
    HTTPOptions = [make_ref()],
    Conf0 = load_pet_fixture(Config, HTTPOptions),
    Result = hackney_response([{body, {ok, jsx:encode(#{})}}]),
    ok = meck:expect(hackney, request, fun(get,
                                           "http://localhost/pets/0",
                                           [],
                                           <<>>,
                                           FunHTTPOptions) ->
        ?assertEqual(HTTPOptions, FunHTTPOptions),
        Result end),

    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Resp = ?MUT:op(Conf1, "find pet by id", [{"id", "0"}]),
    true = meck:validate(hackney),
    ?assertEqual(#{}, Resp),
    ok.

bc_list_operation_with_query_string(Config) ->
    Conf0 = load_pet_fixture(Config),
    Result = hackney_response([{body, {ok, jsx:encode(#{})}}]),

    ok = meck:expect(hackney, request, fun(get,
                                           URL,
                                           RequestHeaders,
                                           Body,
                                           _FunHTTPOptions) ->
        ?assertEqual("http://localhost/pets?limit=5", URL),
        ?assertEqual([], RequestHeaders),
        ?assertEqual(<<>>, Body),
        Result
    end),
    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Resp = ?MUT:op(Conf1, <<"findPets">>, [{"limit", "5"}]),
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
                                           URL,
                                           Headers,
                                           <<>>,
                                           Options) ->
        ?assertEqual("http://localhost/pets/0", URL),
        ?assertEqual([], Headers),
        ?assertEqual([{recv_timeout,infinity}, async], Options),
        async_http_send(self(), Result),
        {ok, Ref} end),

    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Callback = ?MUT:async_op(Conf1, "find pet by id", [{"id", "0"}]),
    Msg = get_msg(),
    Resp = Callback(Msg),
    true = meck:validate(hackney),
    ?assertEqual(#{}, Resp),
    ok.

ea_load_with_http_headers(Config) ->
    PetSwagger = ?config(pet_swagger, Config),
    {ok, PSData} = file:read_file(PetSwagger),
    Result = hackney_response([{body, {ok, PSData}}]),
    Headers = [{"header-1", make_ref()}],
    ok = meck:expect(hackney, request, fun(get,
                                           URL,
                                           RequestHeaders,
                                           <<>>,
                                           _FunHTTPOptions) ->
        ?assertEqual(URL, "http://example.com"),
        ?assertEqual(Headers, RequestHeaders),
        Result
    end),
    ?MUT:load("http://example.com", [{default_headers, Headers}]),
    ok.


eb_get_with_http_headers(Config) ->
    Headers = [make_ref()],
    HTTPOptions = [{default_headers, Headers}],
    Conf0 = load_pet_fixture(Config, HTTPOptions),
    Result = hackney_response([{body, {ok, jsx:encode(#{})}}]),
    ok = meck:expect(hackney, request, fun(get,
                                           URL,
                                           RequestHeaders,
                                           <<>>,
                                           []) ->
        ?assertEqual(URL, "http://localhost/pets/0"),
        ?assertEqual(Headers, RequestHeaders),
        Result end),

    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Resp = ?MUT:op(Conf1, "find pet by id", [{"id", "0"}]),
    true = meck:validate(hackney),
    ?assertEqual(#{}, Resp),
    ok.

ec_async_get_with_http_headers(Config) ->
    Headers = [make_ref()],
    HTTPOptions = [{default_headers, Headers}],
    Conf0 = load_pet_fixture(Config, HTTPOptions),
    Ref = make_ref(),
    Result = {hackney_response, Ref, jsx:encode(#{})},
    ok = meck:expect(hackney, request, fun(get,
                                           URL,
                                           RequestHeaders,
                                           <<>>,
                                           Options) ->
        ?assertEqual("http://localhost/pets/0", URL),
        ?assertEqual(Headers, RequestHeaders),
        ?assertEqual([{recv_timeout,infinity}, async], Options),
        async_http_send(self(), Result),
        {ok, Ref} end),

    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Callback = ?MUT:async_op(Conf1, "find pet by id", [{"id", "0"}]),
    Msg = get_msg(),
    Resp = Callback(Msg),
    true = meck:validate(hackney),
    ?assertEqual(#{}, Resp),
    ok.

fa_get_without_required_param(Config) ->
    Conf0 = load_pet_fixture(Config),

    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Resp = ?MUT:op(Conf1, <<"find pet by id">>, []),
    ?assertEqual({error, missing_required_field, <<"id">>}, Resp),
    ok.

fb_async_get_without_required_param(Config) ->
    Conf0 = load_pet_fixture(Config),

    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Resp = ?MUT:async_op(Conf1, <<"find pet by id">>, []),
    ?assertEqual({error, missing_required_field, <<"id">>}, Resp),
    ok.

ga_post_operation(Config) ->
    Conf0 = load_pet_fixture(Config),
    Result = hackney_response([{body, {ok, jsx:encode(#{})}}]),

    ok = meck:expect(hackney, request, fun(Method,
                                           URL,
                                           RequestHeaders,
                                           Body,
                                           _FunHTTPOptions) ->
        ?assertEqual(post, Method),
        ?assertEqual(URL, "http://localhost/pets"),
        ?assertEqual([], RequestHeaders),
        ?assertEqual(<<"{\"pet\":{\"foo\":\"bar\"}}">>, Body),
        Result
    end),
    Conf1 = ?MUT:set_server(Conf0, "http://localhost"),
    Resp = ?MUT:op(Conf1, <<"addPet">>, [{"pet", [{foo, bar}]}]),
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
     "deletePet",
     "find pet by id",
     "findPets"].

get_msg() ->
    receive
        Msg -> Msg
    after 1000 ->
        error
    end.
