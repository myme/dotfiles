# Build NixOS configuration for all machines under ../machines

{ lib }:

f:

let
  isMachine = { name, type }: type == "directory" || (lib.strings.hasSuffix ".nix" name && name != "default.nix");
  nixFiles = builtins.map (x: x.name) (builtins.filter isMachine
    (lib.mapAttrsToList (name: type: { inherit name type; })
      (builtins.readDir ../machines)));

in builtins.listToAttrs (builtins.map (fname: rec {
  name = lib.strings.removeSuffix ".nix" fname;
  value = f name ../machines/${fname};
}) nixFiles)
