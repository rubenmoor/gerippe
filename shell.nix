with (import <nixpkgs> {});
pkgs.stdenv.mkDerivation {
  buildInputs = [ zlib ];
  name = "env";
}
