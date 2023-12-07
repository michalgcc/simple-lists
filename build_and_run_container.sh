#!/usr/bin/env bash

./build.sh
mkdir -p ./tmp
podman run -it -p 9292:9292 -v ./tmp/:/var/app_db/ localhost/simple-lists:latest