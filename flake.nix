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
  };

  outputs = { self, haskell-nix, hackage-nix, nixpkgs }:
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
              # libpg-query's generated protobuf modules match libpg_query
              # 18; each major renumbers the AST's node types, so another
              # version decodes statements into the wrong nodes.
              (_: prev: {
                libpg_query = prev.libpg_query.overrideAttrs (old: {
                  version = "18.0.0";
                  src = prev.fetchFromGitHub {
                    owner = "pganalyze";
                    repo = "libpg_query";
                    tag = "18.0.0";
                    hash = "sha256-Fs9SFs8ramKYdkv1gEOMJd9SnLmKDcbf+zYKv1hHBfc=";
                  };
                  # The dylib's install name is its bare file name, which
                  # dyld cannot find at run time.
                  nativeBuildInputs = (old.nativeBuildInputs or [ ])
                    ++ prev.lib.optional prev.stdenv.isDarwin prev.fixDarwinDylibNames;
                  # pg_query.h includes postgres_deparse.h, which nixpkgs
                  # does not install. The library is built under its bare
                  # name but records a versioned soname/install name, which
                  # nothing provides; make names both.
                  postInstall = (old.postInstall or "") + ''
                    install -Dm644 postgres_deparse.h -t "$out/include"
                    names=$(make -s --eval='__pg_query_names: ; @echo $(SONAME) $(SOLIBVER) $(SOLIB)' __pg_query_names)
                    set -- $names
                    soname=$1
                    solibver=$2
                    solib=$3
                    cd "$out/lib"
                    [ "$soname" = "$solib" ] || ln -s "$solib" "$soname"
                    [ "$solibver" = "$solib" ] || [ "$solibver" = "$soname" ] || ln -s "$solib" "$solibver"
                  '';
                });
              })
              (final: _: { pg_query = final.libpg_query; })
            ];
          };

          project = pkgs.haskell-nix.project {
            src = ./.;
            compiler-nix-name = "ghc9124";
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
