{ callPackage }:
{
  allMachines = callPackage ./allMachines.nix { };
  makeNixOS = import ./makeNixOS.nix;
}
