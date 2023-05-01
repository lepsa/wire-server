# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, base
, cassandra-util
, conduit
, galley
, gitignoreSource
, imports
, lens
, lib
, optparse-applicative
, tinylog
, types-common
, unliftio
, wire-api
}:
mkDerivation {
  pname = "migrate-sso-feature-flag";
  version = "1.0.0";
  src = gitignoreSource ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    cassandra-util
    conduit
    galley
    imports
    lens
    optparse-applicative
    tinylog
    types-common
    unliftio
    wire-api
  ];
  description = "Backfill sso feature flag into teams that already have an IdP";
  license = lib.licenses.agpl3Only;
  mainProgram = "migrate-sso-feature-flag";
}
