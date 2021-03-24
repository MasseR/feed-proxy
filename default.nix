{ mkDerivation, aeson, base, bytestring, conduit, containers
, directory, exceptions, feed, filepath, generic-lens, hspec
, html-conduit, http-client-tls, http-conduit, http-media, lens
, lens-aeson, mtl, network-uri, optparse-generic, QuickCheck
, servant, servant-server, stdenv, text, time, transformers, wai
, warp, xml-conduit, xml-lens, xml-types
}:
mkDerivation {
  pname = "feed-proxy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring conduit containers directory exceptions feed
    filepath generic-lens html-conduit http-conduit http-media lens
    lens-aeson mtl network-uri servant servant-server text time
    transformers wai warp xml-conduit xml-lens xml-types
  ];
  executableHaskellDepends = [
    base http-client-tls optparse-generic
  ];
  testHaskellDepends = [ base hspec QuickCheck ];
  license = stdenv.lib.licenses.bsd3;
}
