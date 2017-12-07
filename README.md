# genome-api
`genome` is a project for micro-service-oriented companies to visualize their eco-system of services

## Components

| name | description | techs |
|---|---|---|
| genome-api | RESTful API, exposes all the logic for managing the database | `Haskell`, `Servant` |
| database | graph database + out of the box visualisation | `Neo4j` | X |
 
## Getting started

**Start the DB**
```bash
export GENOME_ENV=dev
docker-compose up neo4j_db
```

**Creating the constraints and importing test data**
```bash
./neo4j/init_db.sh
```

The Database's data and logs will be persisted under `${HOME}/genome-api/neo4j-persist/${GENOME_ENV}/`, you can reset your development data by running: `rm -rf ${HOME}/genome-api/neo4j-persist/dev/`

**Building and starting the web server**
```bash
stack build
stack exec genome-api
```
By default the server starts at `localhost:8081`

**Running the tests**
```bash
./deploy/run_tests.sh
```

### Trying out the API with curl
```bash
#  Person API
curl -i -X PUT -H "Content-Type: application/json" -d '{"name": "Daniel Mateus Pires", "role": "Software & Data Engineer", "slack": "@dpires", "email": "dpires@gilt.com"}' localhost:8081/person/dpires@gilt.com
curl -i -X PUT -H "Content-Type: application/json" -d '{"teams":["DataTeam", "AnotherTeam"]}' localhost:8081/person/dpires@gilt.com/teams

#  Team API 
curl -i -X PUT -H "Content-Type: application/json" -d '{"name": "Cerebro", "role": "Personalization & all things data science", "slackChannel": "#team-personalization"}' localhost:8081/team/Cerebro
curl -i -X PUT -H "Content-Type: application/json" -d '{"microServices":["svc-event"]}' localhost:8081/team/Cerebro/microServices
```
