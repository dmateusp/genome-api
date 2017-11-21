# genome-api
`genome` is a project for micro-service-oriented companies to visualize their eco-system of services

## Components

| name | description | techs |
|---|---|---|
| genome-api | RESTful API, exposes all the logic for managing the database | `Haskell`, `Servant` |
| database | graph database + out of the box visualisation | `Neo4j` | X |
 
## Getting started

**Start the DB and change the password**
```
export GENOME_ENV=dev
docker-compose up db
curl -H "Content-Type: application/json" -XPOST -d '{"password":"devenv"}' -u neo4j:neo4j http://192.168.99.100:7474/user/neo4j/password
```
The Database's data and logs will be persisted under `${HOME}/genome-api/neo4j-persist/${GENOME_ENV}/`

**Build**
```
stack build
```
**Start the web server locally**
```
stack exec genome-api
```

**Run the tests**
```
./deploy/run_tests.sh
```

### Trying out the API with curl
```
curl -i -X GET -H "Content-Type: application/json" localhost:8081/persons

curl -i -X PUT -H "Content-Type: application/json" -d '{"name": "Daniel Mateus Pires", "role": "Software & Data Engineer", "slack": "@dpires", "email": "dpires@gilt.com"}' localhost:8081/person
```
