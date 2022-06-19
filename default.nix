let
  compiler = "ghc884";

  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = self: super: {
              mkDerivation = args: super.mkDerivation ( args // {
                doCheck = false;
                doHaddock = false;
                enableLibraryProfiling = false;
              });
            };
          };
        };
      };
    };
    allowBroken = true;
  };

  pkgs = import <nixpkgs> { inherit config; };
  drv = pkgs.haskell.packages."${compiler}".callCabal2nix "gerippe" ./. { };

  easy-hls = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "jkachmar";
    repo = "easy-hls-nix";
    rev = "db85cac9d0405b4769b75cba0b004aed3beaf2de";
    sha256 = "10nff6mqflrd6dz1fp2l9vmfwbgk0r7zm81qh2xnjj19a47pd7v3";
  }) {
    ghcVersions = [ "8.8.4" ];
  };

  env =
    # don't know why, but the haskell-language doesn't seem to
    # be a build tool, but a native build input
    #
    # with pkgs.haskell.lib;
    # addBuildTools drv (
    #   with pkgs.haskellPackages;
    #   [ haskell-language-server ]
    # );
    with pkgs.haskellPackages;
    drv.env.overrideAttrs ( oldAttrs: rec {
      nativeBuildInputs =
        oldAttrs.nativeBuildInputs ++ [
          cabal-install
          easy-hls
        ];
    });
in
  if pkgs.lib.inNixShell then env else drv
