# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation, base, containers, gitignoreSource, imports
, insert-ordered-containers, language-dot, lens, lib, openapi3
, text, wire-api
}:
mkDerivation {
  pname = "fedcalls";
  version = "1.0.0";
  src = gitignoreSource ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers imports insert-ordered-containers language-dot lens
    openapi3 text wire-api
  ];
  description = "Generate a dot file from swagger docs representing calls to federated instances";
  license = lib.licenses.agpl3Only;
  mainProgram = "fedcalls";
}
