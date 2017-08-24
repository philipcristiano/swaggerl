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
