# genome-api
`genome` is a project for micro-service-oriented companies to visualize their eco-system of services


**Key questions to answer**:
* which micro-services exist
* who owns `x` service
* what does service `x` do

## Components

The project is divided in **2** main components (+ a database):

| name | description | techs | repo
|---|---|---|---|
| genome-api | RESTful API, exposes all the logic for managing the database | `Haskell`, `Servant` | [here](https://github.com/dmateusp/genome-api) |
| genome-cli | a command line interface talking to `genome-api` | `Haskell` | [here](https://github.com/dmateusp/genome-cli) |
| database | graph database + out of the box visualisation | `Neo4j` | X |
 
## Development workflow

A docker-compose file is provided which has `nginx`, builds and starts `genome-api` and also has `neo4j` with some dummy data!

```
docker-compose up neo4j
stack test
stack build
stack exec genome-api
```

### Ghci notes
To use `Data.Text`: do `:set -XOverloadedStrings`
## TODO
Everything