#!/usr/bin/env bash
set -e

nix flake lock --recreate-lock-file
direnv reload