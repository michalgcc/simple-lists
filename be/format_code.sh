#!/usr/bin/env bash

# pg_format -i -f 3 --no-extra-line  ./db/migrations/*.sql
hpack
find . \( -path "./dist-*" -prune \) -o -type f -name "*.hs" -exec ormolu --mode inplace {} +
stylish-haskell -ri src/

