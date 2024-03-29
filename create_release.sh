#!/usr/bin/env bash
set -e

git checkout main
git pull --ff-only
cog bump --auto
git push origin $(cog get-version)
