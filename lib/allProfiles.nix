# Build NixOS configuration for all machines under ../machines

{ lib }:

dir: f:

let
  allProfiles = builtins.filter (m: m != "default.nix") (lib.myme.allNixFiles dir);

in builtins.listToAttrs (builtins.map (fname: rec {
  name = lib.strings.removeSuffix ".nix" fname;
  value = f name (import /${dir}/${fname});
}) allProfiles)
