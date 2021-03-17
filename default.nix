{ mkDerivation, base, hspec, http-client, lens, mtl, QuickCheck
, stdenv, transformers, xml-conduit, xml-lens
}:
mkDerivation {
  pname = "feed-proxy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base http-client lens mtl transformers xml-conduit xml-lens
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec QuickCheck ];
  license = stdenv.lib.licenses.bsd3;
}
