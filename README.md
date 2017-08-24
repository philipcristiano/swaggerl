# Swaggerl

A minimal Swagger client in Erlang. At the moment attempts to be useful in personal use-cases without trying to be spec-compliant.


## Example

```
Spec = swaggerl:load("http://petstore.swagger.io/v2/swagger.json").
API = swaggerl:set_server("http://petstore.swagger.io").
Result = swaggerl:op(API, "getPetById", [{"petId", 0}]).
```
