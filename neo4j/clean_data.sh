#!/usr/bin/env bash

# Use me to clean up your data/logs

# 'test' | 'dev'
GENOME_ENV=$1

rm -rf ${HOME}/genome-api/neo4j-persist/${GENOME_ENV}/data
