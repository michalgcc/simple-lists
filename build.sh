#!/usr/bin/env bash

rm ./prod/ -rf
mkdir -p ./prod/static
mkdir -p ./prod/migrations

cp start.sh ./prod

pushd fe
./build.sh
cp ./prod/* ../prod/static
popd
pushd be
./build.sh
cp ./prod/* ../prod
cp ./db/migrations/* ../prod/migrations
popd

nix build path://$(pwd)/.#dockerContainer