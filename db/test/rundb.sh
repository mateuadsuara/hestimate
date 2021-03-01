#!/bin/bash

docker build -t hestimate-postgres-test-image .
docker stop hestimate-postgres-test-container
docker rm hestimate-postgres-test-container
docker run -d --name hestimate-postgres-test-container -p 5432:5432 hestimate-postgres-test-image
