# Build NixOS configuration for all machines under ../machines

{ lib }:

pred: apply:

let
  allProfiles = builtins.filter (m: m != "default.nix") (lib.myme.allNixFiles ../machines);
  doImport = fname: {
    name = lib.strings.removeSuffix ".nix" fname;
    value = import ../machines/${fname};
  };
  imported = builtins.filter
    ({ name, value }: pred name value)
    (builtins.map doImport allProfiles);
  toAttr = { name, value }: {
    inherit name;
    value = apply name value;
  };
  attributes = builtins.map toAttr imported;

in builtins.listToAttrs attributes
