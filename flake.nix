{
  inputs = {
    nixpkgs-old.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, nixpkgs-old, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { system = system; };
        old-pkgs = import nixpkgs-old {};
        shmigSQLite = (pkgs.shmig.override {
          withMySQL = false;
          withPSQL = false;
          withSQLite = true;
        });
      in with pkgs; {
        packages.dockerContainer = pkgs.dockerTools.buildLayeredImage {
          name = "simple-lists";
          tag = "latest";
          config = {
            Cmd = [ "./start.sh" ];
            WorkingDir = "${./prod}";
            ExposedPorts = { "9292/tcp" = { }; };
            Volumes = { "/var/app_db/" = { }; };
            Env = [
              "DB_CONN_STRING=/var/app_db/simple-lists.db"
              "PORT=9292"
              "ALLOWED_CORS_ORIGINS_COMMA_SEPARATED=http://localhost:9292"
            ];
          };
          contents = [
            pkgs.bash
            pkgs.coreutils
            pkgs.lzma
            pkgs.sqlite
            pkgs.zlib
            pkgs.gmp
            pkgs.libffi
            shmigSQLite
          ];
        };

        devShells.default = mkShell {
          buildInputs = [
            # BE:
            haskellPackages.cabal-install
            # Additional deps:
            haskellPackages.ormolu
            haskellPackages.stylish-haskell
            haskellPackages.haskell-debug-adapter
            haskellPackages.haskell-language-server
            haskellPackages.hpack
            # Db migration tool
            shmigSQLite

            # Native dependencies:
            zlib
            lzma
            sqlite

            # FE:
            nodejs
            dhall-lsp-server
            purescript
            old-pkgs.spago
            esbuild
            nodePackages.purs-tidy
            # Optional for nvim
            nodePackages.purescript-language-server

            # Utils
            nixfmt
            cocogitto
          ];

          shellHook = ''
            yes | cog install-hook --all
          '';
        };
      });
}

