{ mkDerivation, aeson, annotated-exception, base, bytestring
, conduit, containers, directory, ekg-core, exceptions, feed
, filepath, generic-lens, hspec, html-conduit, http-client-tls
, http-conduit, http-media, katip, lens, lens-aeson, lib, mtl
, network-uri, optparse-generic, QuickCheck, servant
, servant-server, stm, text, time, transformers, unliftio
, unordered-containers, wai, wai-middleware-metrics, warp
, xml-conduit, xml-lens, xml-types
}:
mkDerivation {
  pname = "feed-proxy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson annotated-exception base bytestring conduit containers
    directory ekg-core exceptions feed filepath generic-lens
    html-conduit http-conduit http-media katip lens lens-aeson mtl
    network-uri servant servant-server stm text time transformers
    unliftio unordered-containers wai wai-middleware-metrics warp
    xml-conduit xml-lens xml-types
  ];
  executableHaskellDepends = [
    base http-client-tls katip optparse-generic
  ];
  testHaskellDepends = [ base hspec QuickCheck ];
  license = lib.licenses.bsd3;
  mainProgram = "feed-proxy";
}
