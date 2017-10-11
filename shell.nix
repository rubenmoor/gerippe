{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, esqueleto, persistent
      , stdenv, transformers
      }:
      mkDerivation {
        pname = "gerippe";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base containers esqueleto persistent transformers
        ];
        homepage = "http://github.com/rubenmoor/gerippe#readme";
        description = "Initial project template from stack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
