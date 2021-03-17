{ mkDerivation, base, bytestring, conduit, containers, directory
, feed, hspec, http-conduit, lens, mtl, network-uri, QuickCheck
, stdenv, text, time, transformers, xml-conduit, xml-lens
, xml-types
}:
mkDerivation {
  pname = "feed-proxy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring conduit containers directory feed http-conduit lens
    mtl network-uri text time transformers xml-conduit xml-lens
    xml-types
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec QuickCheck ];
  license = stdenv.lib.licenses.bsd3;
}
