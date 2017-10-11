{ mkDerivation, base, containers, esqueleto, persistent, stdenv
, transformers
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
}
