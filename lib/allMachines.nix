# Build NixOS configuration for all machines under ../machines

{ lib }:

args:

let
  isNixFile = p: lib.strings.hasSuffix ".nix" p && p != "default.nix";
  nixFiles = builtins.filter isNixFile
    (builtins.attrNames (builtins.readDir ../machines));

in builtins.listToAttrs (builtins.map (fname: rec {
  name = lib.strings.removeSuffix ".nix" fname;
  value = import ./makeNixOS.nix name args;
}) nixFiles)
