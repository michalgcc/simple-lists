#!/usr/bin/env bash
set -e

current_dir=$(pwd)

rm ./static/ -rf
mkdir -p ./static

pushd ../fe
./build.sh
cp  ./prod/* "$current_dir/static"
popd

cabal run
