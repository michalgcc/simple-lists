#!/usr/bin/env bash

./build.sh
podman load < result
mkdir -p ./tmp
podman run -it -p 9292:9292 -v ./tmp/:/var/app_db/ localhost/simple-lists:latest