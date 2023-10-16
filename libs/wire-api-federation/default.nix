# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, aeson-pretty
, amqp
, async
, base
, bytestring
, bytestring-conversion
, containers
, exceptions
, gitignoreSource
, HsOpenSSL
, hspec
, hspec-discover
, http-media
, http-types
, http2
, http2-manager
, HUnit
, imports
, kan-extensions
, lens
, lib
, metrics-wai
, mtl
, openapi3
, QuickCheck
, schema-profunctor
, servant
, servant-client
, servant-client-core
, servant-server
, singletons
, singletons-th
, text
, time
, transformers
, transitive-anns
, types-common
, uuid
, wai-utilities
, wire-api
}:
mkDerivation {
  pname = "wire-api-federation";
  version = "0.1.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    aeson
    amqp
    async
    base
    bytestring
    bytestring-conversion
    containers
    exceptions
    HsOpenSSL
    http-media
    http-types
    http2
    http2-manager
    imports
    kan-extensions
    lens
    metrics-wai
    mtl
    openapi3
    QuickCheck
    schema-profunctor
    servant
    servant-client
    servant-client-core
    servant-server
    singletons-th
    text
    time
    transformers
    transitive-anns
    types-common
    wai-utilities
    wire-api
  ];
  testHaskellDepends = [
    aeson
    aeson-pretty
    base
    bytestring
    containers
    hspec
    HUnit
    imports
    QuickCheck
    singletons
    time
    types-common
    uuid
    wire-api
  ];
  testToolDepends = [ hspec-discover ];
  description = "The Wire server-to-server API for federation";
  license = lib.licenses.agpl3Only;
}
