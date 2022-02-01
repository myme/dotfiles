# Build NixOS configuration for all machines under ../machines

{ lib, pkgs }:

f:

let
  allMachines = builtins.filter (m: m != "default.nix") (pkgs.myme.lib.allNixFiles ../machines);

in builtins.listToAttrs (builtins.map (fname: rec {
  name = lib.strings.removeSuffix ".nix" fname;
  value = f name ../machines/${fname};
}) allMachines)
