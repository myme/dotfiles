# Build NixOS configuration for all machines under ../machines

{ lib, pkgs }:

dir: f:

let
  allProfiles = builtins.filter (m: m != "default.nix") (pkgs.myme.lib.allNixFiles dir);

in builtins.listToAttrs (builtins.map (fname: rec {
  name = lib.strings.removeSuffix ".nix" fname;
  value = f name /${dir}/${fname};
}) allProfiles)
