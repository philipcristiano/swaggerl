# Swaggerl

A minimal Swagger client in Erlang. At the moment attempts to be useful in personal use-cases without trying to be spec-compliant.


## Example

```
Spec = swaggerl:load("http://petstore.swagger.io/v2/swagger.json").
API = swaggerl:set_server("http://petstore.swagger.io").

swaggerl:operations(Spec).
["addPet","createUser","createUsersWithArrayInput",
 "createUsersWithListInput","deleteOrder","deletePet",
 "deleteUser","findPetsByStatus","findPetsByTags",
 "getInventory","getOrderById","getPetById","getUserByName",
 "loginUser","logoutUser","placeOrder","updatePet",
 "updatePetWithForm","updateUser","uploadFile"]

Result = swaggerl:op(API, "getPetById", [{"petId", 0}]).
```


## Async

Async operations can be made with `swaggerl:async_op/3`. It has the same
arguments as `swagger:op/3` but returns a callback that will parse messages
passed to it.

```
Spec = swaggerl:load("http://petstore.swagger.io/v2/swagger.json").
API = swaggerl:set_server("http://petstore.swagger.io").

swaggerl:operations(Spec).
["addPet","createUser","createUsersWithArrayInput",
 "createUsersWithListInput","deleteOrder","deletePet",
 "deleteUser","findPetsByStatus","findPetsByTags",
 "getInventory","getOrderById","getPetById","getUserByName",
 "loginUser","logoutUser","placeOrder","updatePet",
 "updatePetWithForm","updateUser","uploadFile"]

Callback = swaggerl:async_op(API, "getPetById", [{"petId", 0}]).

Msg = receive
    M -> M
after 10000 ->
    error
end.

Result = Callback(Msg).

Other HTTP options cannot be passed through yet. There is an `infinity` timeout
set explicitly for async operations. This will change in the future.

# Defaults

Defaults for requests can be set when first `load`ing.

Options -

* `default_headers` - Applied to each request, such as authorization headers.

```erlang
swaggerl:load("http://petstore.swagger.io/v2/swagger.json", [{default_headers, [{<<"x-foo">>, <<"foo">>}]}]).
```
