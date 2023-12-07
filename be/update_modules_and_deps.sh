#!/usr/bin/env nix-shell
#!nix-shell -i bash -p haskellPackages.hpack haskellPackages.cabal2nix

# Run hpack to generate the .cabal file from package.yaml
hpack
cabal build
cabal freeze
# Run cabal2nix to generate default.nix from the .cabal file
# cabal2nix . > default.nix
