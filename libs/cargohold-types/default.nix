# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation, base, bytestring-conversion, gitignoreSource
, imports, lib, types-common, wire-api
}:
mkDerivation {
  pname = "cargohold-types";
  version = "1.5.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    base bytestring-conversion imports types-common wire-api
  ];
  description = "Asset Storage API Types";
  license = lib.licenses.agpl3Only;
}
