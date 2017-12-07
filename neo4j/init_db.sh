#!/usr/bin/env bash

# This script creates constraints and inserts a few example nodes

# UNIQUE constraints
docker cp constraints.cypher neo4j_db:/constraints.cypher
docker exec -it neo4j_db bash -c "cat /constraints.cypher | bin/cypher-shell"

# STARTING data
docker cp starterData.cypher neo4j_db:/starterData.cypher
docker exec -it neo4j_db bash -c "cat /starterData.cypher | bin/cypher-shell"