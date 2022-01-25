# Build NixOS configuration for all machines under ../machines

{ lib, pkgs }:

f:

builtins.listToAttrs (builtins.map (fname: rec {
  name = lib.strings.removeSuffix ".nix" fname;
  value = f name ../machines/${fname};
}) (pkgs.myme.lib.allNixFiles ../machines))
