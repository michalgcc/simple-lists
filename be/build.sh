#!/usr/bin/env bash
set -e

rm ./prod/ -rf
cabal new-clean
cabal install --installdir "$(pwd)/prod" --install-method=copy --overwrite-policy=always
