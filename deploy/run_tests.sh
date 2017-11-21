#!/usr/bin/env bash

export GENOME_ENV=test
# -- Functions
function waitForDb() {
  (curl -sSf -u neo4j:neo4j http://192.168.99.100:7474/db/data > /dev/null && echo "DB is ready!") \
   || (echo "DB not ready yet.." && sleep 2 && waitForDb)
}

# -- Preparing Docker
docker-machine start
eval $(docker-machine env)
docker-compose up -d db

# -- Waiting for DB to start
waitForDb

# -- Peparing the DB
# Changing the password
curl -H "Content-Type: application/json" -X POST -d '{"password":"testenv"}' -u neo4j:neo4j http://192.168.99.100:7474/user/neo4j/password
# Adding test data
curl -H "Content-Type: application/json" -X POST --data-binary @./deploy/persons.json -u neo4j:testenv http://192.168.99.100:7474/db/data/cypher > /dev/null

# -- Testing
stack test

# -- Tearing down env
docker-compose down
rm -rf ${HOME}/genome-api/neo4j-persist/${GENOME_ENV}

