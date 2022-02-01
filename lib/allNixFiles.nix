# Enumerate all .nix files (or directories) under dir

{ lib }:

dir:

let
  isNixFile = { name, type }: type == "directory" || lib.strings.hasSuffix ".nix" name;

in builtins.map (x: x.name) (builtins.filter isNixFile
  (lib.mapAttrsToList (name: type: { inherit name type; })
    (builtins.readDir dir)))
