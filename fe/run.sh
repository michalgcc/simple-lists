#!/usr/bin/env bash
set -e

pgrep -f parcel > /dev/null && pgrep -f parcel | xargs kill -9
rm dist -rf
env PRODUCTION=False spago build
npm run serve
