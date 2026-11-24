{
  description = "Lint sqitch migrations for PostgreSQL lock and rewrite hazards";

  # What CI builds is pushed here (.github/workflows/ci.yml).
  nixConfig = {
    extra-substituters = "https://kronor-open.cachix.org";
    extra-trusted-public-keys = "kronor-open.cachix.org-1:D1shHZh5BRkmM8RB9BaEqBURIgD/n5+u8KFXD1+DbF8=";
  };

  inputs = {
    haskell-nix.url = "github:kronor-io/haskell.nix/nixpkgs-ghc-601a4d19";
    nixpkgs.url = "github:NixOS/nixpkgs/release-26.05";
    haskell-nix.inputs.hackage.follows = "hackage-nix";
    haskell-nix.inputs.nixpkgs.follows = "nixpkgs";
    hackage-nix.url = "github:kronor-io/hackage.nix";
    hackage-nix.flake = false;
    kronor-haskell-packages.url = "github:kronor-io/kronor-haskell-packages/repo";
    kronor-haskell-packages.flake = false;
  };

  outputs = { self, haskell-nix, hackage-nix, kronor-haskell-packages, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      perSystem = system:
        let
          haskellNix = import haskell-nix {
            inherit system;
            sourcesOverride.hackage = hackage-nix;
          };

          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              (final: _: { postgresql = final.postgresql_18; })
              haskellNix.overlay
              # pg-query was generated against libpg_query 16's
              # protobuf schema; 17 renumbered fields, so a newer library
              # decodes statements into the wrong AST nodes.
              (_: prev: {
                libpg_query = prev.libpg_query.overrideAttrs (old: {
                  version = "16-5.2.0";
                  src = prev.fetchFromGitHub {
                    owner = "pganalyze";
                    repo = "libpg_query";
                    tag = "16-5.2.0";
                    hash = "sha256-UziczfuJ0d6tYi87EC/HDDC5nbe3YMliLFZBTaYucj0=";
                  };
                  # The library's install name is its bare file name, which
                  # dyld cannot find at run time.
                  nativeBuildInputs = (old.nativeBuildInputs or [ ])
                    ++ prev.lib.optional prev.stdenv.isDarwin prev.fixDarwinDylibNames;
                  # The library's soname is libpg_query.so.1605.1, but nixpkgs
                  # installs it as libpg_query.so alone, so nothing linked
                  # against it finds it at run time.
                  postInstall = (old.postInstall or "") + prev.lib.optionalString prev.stdenv.isLinux ''
                    ln -s libpg_query.so $out/lib/libpg_query.so.1605.1
                  '';
                });
              })
              (final: _: { pg_query = final.libpg_query; })
            ];
          };

          project = pkgs.haskell-nix.project {
            src = ./.;
            compiler-nix-name = "ghc9124";
            inputMap = {
              "https://kronor-io.github.io/kronor-haskell-packages" = kronor-haskell-packages;
            };
            modules = [{
              packages.sqint.components.exes.sqint.build-tools = [ pkgs.installShellFiles ];
              packages.sqint.components.exes.sqint.postInstall = ''
                $out/bin/sqint --bash-completion-script $out/bin/sqint > sqint.bash
                $out/bin/sqint --zsh-completion-script $out/bin/sqint > sqint.zsh
                $out/bin/sqint --fish-completion-script $out/bin/sqint > sqint.fish
                installShellCompletion --cmd sqint \
                  --bash sqint.bash \
                  --zsh sqint.zsh \
                  --fish sqint.fish
              '';
            }];
          };

          sqint = project.sqint.components.exes.sqint;
          sqint-test = project.sqint.checks.sqint-test;

          # sqint with what its dynamic pass runs: sqitch, and psql under it.
          dockerImage = pkgs.dockerTools.buildLayeredImage {
            name = "sqint";
            tag = "latest";
            contents = [
              sqint
              pkgs.sqitchPg
              pkgs.postgresql_18
              pkgs.cacert
              pkgs.dockerTools.fakeNss
            ];
            extraCommands = ''
              mkdir -p work tmp
              chmod 1777 tmp
            '';
            config = {
              Entrypoint = [ "/bin/sqint" ];
              WorkingDir = "/work";
              Env = [ "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];
            };
          };
        in
        {
          inherit pkgs project sqint sqint-test dockerImage;
        };

      # What CI builds on each system, as a GitHub Actions matrix: one job per
      # package and system, as kronor's other public packages do.
      githubPlatforms = {
        "x86_64-linux" = "ubuntu-latest";
        "aarch64-linux" = "ubuntu-24.04-arm";
        "aarch64-darwin" = "macos-latest";
      };

      bySystem = forAllSystems perSystem;
    in
    {
      packages = forAllSystems (system: rec {
        sqint = bySystem.${system}.sqint;
        default = sqint;
      } // nixpkgs.lib.optionalAttrs (nixpkgs.lib.hasSuffix "linux" system) {
        dockerImage = bySystem.${system}.dockerImage;
      });

      checks = forAllSystems (system: {
        sqint-test = bySystem.${system}.sqint-test;
      });

      githubActions.matrix.include = nixpkgs.lib.concatMap
        (system:
          map
            (name: {
              inherit name system;
              os = githubPlatforms.${system};
              attr = "packages.${system}.${name}";
            })
            (builtins.attrNames (builtins.removeAttrs self.packages.${system} [ "default" ]))
          ++ [{
            name = "sqint-test";
            inherit system;
            os = githubPlatforms.${system};
            attr = "checks.${system}.sqint-test";
          }])
        systems;

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${bySystem.${system}.sqint}/bin/sqint";
        };
      });

      devShells = forAllSystems (system:
        let inherit (bySystem.${system}) pkgs project; in {
          default = project.shellFor {
            tools.cabal = { version = "3.16.1.0"; };
            tools.fourmolu = { version = "0.19.0.1"; };
            tools.hlint = { version = "3.10"; };
            buildInputs = [ pkgs.libpg_query pkgs.postgresql_18 pkgs.sqitchPg pkgs.protobuf ];
          };
        });
    };
}
