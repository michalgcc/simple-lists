#!/usr/bin/env bash
set -e

nix flake lock --update-input nixpkgs --update-input unstable
direnv reload