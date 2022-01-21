# Build NixOS configuration for all machines under ../machines

{ lib }:

f:

let
  isNixFile = p: lib.strings.hasSuffix ".nix" p && p != "default.nix";
  nixFiles = builtins.filter isNixFile
    (builtins.attrNames (builtins.readDir ../machines));

in builtins.listToAttrs (builtins.map (fname: rec {
  name = lib.strings.removeSuffix ".nix" fname;
  value = f name ../machines/${name}.nix;
}) nixFiles)
