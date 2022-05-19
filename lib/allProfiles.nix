# Build NixOS configuration for all machines under ../machines

{ lib }:

dir: f:

let
  allProfiles = builtins.filter (m: m != "default.nix") (lib.allNixFiles dir);

in builtins.listToAttrs (builtins.map (fname: rec {
  name = lib.strings.removeSuffix ".nix" fname;
  value = f name /${dir}/${fname};
}) allProfiles)
