# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, aeson-pretty
, aeson-qq
, async
, attoparsec
, base
, base64-bytestring
, binary
, binary-parsers
, bytestring
, bytestring-arbitrary
, bytestring-conversion
, case-insensitive
, cassandra-util
, cassava
, cereal
, comonad
, conduit
, constraints
, containers
, cookie
, cryptonite
, currency-codes
, deriving-aeson
, deriving-swagger2
, either
, email-validate
, errors
, extended
, extra
, filepath
, generics-sop
, ghc-prim
, gitignoreSource
, hashable
, hex
, hostname-validate
, hscim
, HsOpenSSL
, hspec
, hspec-wai
, http-api-data
, http-client
, http-media
, http-types
, imports
, insert-ordered-containers
, iproute
, iso3166-country-codes
, iso639
, jose
, lens
, lib
, memory
, metrics-wai
, mime
, mtl
, pem
, polysemy
, pretty
, process
, proto-lens
, protobuf
, QuickCheck
, quickcheck-instances
, random
, resourcet
, retry
, saml2-web-sso
, schema-profunctor
, scientific
, scrypt
, servant
, servant-client
, servant-client-core
, servant-conduit
, servant-multipart
, servant-server
, servant-swagger
, singletons
, singletons-base
, singletons-th
, sop-core
, swagger2
, tagged
, tasty
, tasty-hspec
, tasty-hunit
, tasty-quickcheck
, text
, time
, tinylog
, transitive-anns
, types-common
, unliftio
, unordered-containers
, uri-bytestring
, utf8-string
, uuid
, vector
, wai
, wai-extra
, wai-utilities
, wai-websockets
, websockets
, wire-message-proto-lens
, x509
, zauth
}:
mkDerivation {
  pname = "wire-api";
  version = "0.1.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    aeson
    async
    attoparsec
    base
    base64-bytestring
    binary
    binary-parsers
    bytestring
    bytestring-conversion
    case-insensitive
    cassandra-util
    cassava
    cereal
    comonad
    conduit
    constraints
    containers
    cookie
    cryptonite
    currency-codes
    deriving-aeson
    deriving-swagger2
    either
    email-validate
    errors
    extended
    extra
    filepath
    generics-sop
    ghc-prim
    hashable
    hostname-validate
    hscim
    HsOpenSSL
    http-api-data
    http-client
    http-media
    http-types
    imports
    insert-ordered-containers
    iproute
    iso3166-country-codes
    iso639
    jose
    lens
    memory
    metrics-wai
    mime
    mtl
    pem
    polysemy
    proto-lens
    protobuf
    QuickCheck
    quickcheck-instances
    random
    resourcet
    retry
    saml2-web-sso
    schema-profunctor
    scientific
    scrypt
    servant
    servant-client
    servant-client-core
    servant-conduit
    servant-multipart
    servant-server
    servant-swagger
    singletons
    singletons-base
    singletons-th
    sop-core
    swagger2
    tagged
    text
    time
    tinylog
    transitive-anns
    types-common
    unordered-containers
    uri-bytestring
    utf8-string
    uuid
    vector
    wai
    wai-extra
    wai-utilities
    wai-websockets
    websockets
    wire-message-proto-lens
    x509
    zauth
  ];
  testHaskellDepends = [
    aeson
    aeson-pretty
    aeson-qq
    async
    base
    binary
    bytestring
    bytestring-arbitrary
    bytestring-conversion
    cassava
    containers
    cryptonite
    currency-codes
    either
    filepath
    hex
    hscim
    hspec
    hspec-wai
    http-types
    imports
    iso3166-country-codes
    iso639
    lens
    memory
    metrics-wai
    pem
    pretty
    process
    proto-lens
    QuickCheck
    schema-profunctor
    servant
    servant-server
    swagger2
    tasty
    tasty-hspec
    tasty-hunit
    tasty-quickcheck
    text
    time
    types-common
    unliftio
    uri-bytestring
    uuid
    vector
    wai
    wire-message-proto-lens
  ];
  license = lib.licenses.agpl3Only;
}
