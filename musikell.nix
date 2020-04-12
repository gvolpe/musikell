{ mkDerivation, aeson, async, base, bytestring, containers, dhall
, exceptions, hasbolt, lens, morpheus-graphql, mtl, refined
, resource-pool, scotty, stdenv, text, wreq
}:
mkDerivation {
  pname = "musikell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring containers dhall exceptions hasbolt
    lens morpheus-graphql mtl refined resource-pool scotty text wreq
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/gvolpe/musikell";
  description = "Artists, Albums and Songs";
  license = stdenv.lib.licenses.asl20;
}
