# genome-api
`genome` is a project for micro-service-oriented companies to visualize their eco-system of services


**Key questions to answer**:
* which micro-services exist
* who owns `x` service
* what does service `x` do

## Components

| name | description | techs | repo
|---|---|---|---|
| genome-api | RESTful API, exposes all the logic for managing the database | `Haskell`, `Servant` | [here](https://github.com/dmateusp/genome-api) |
| database | graph database + out of the box visualisation | `Neo4j` | X |
 
## Development workflow

A docker-compose file is provided which has `nginx`, builds and starts `genome-api` and also has `neo4j` with some dummy data!

```
docker-compose up db
curl -H "Content-Type: application/json" -XPOST -d '{"password":"devenv"}' -u neo4j:neo4j http://192.168.99.100:7474/user/neo4j/password

stack test
stack build
stack exec genome-api
```

### Ghci notes
To use `Data.Text`: do `:set -XOverloadedStrings`

### Testing the API
```
curl -X GET -H "Content-Type: application/json" localhost:8081/persons

curl -i -X PUT -H "Content-Type: application/js: "Daniel", "role": "Software & Data Engineer", "slack": "@dpires", "email": "dpires@gilt.com"}' localhost:8081/person
```
