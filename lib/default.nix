{ callPackage }:
{
  allNixFiles = callPackage ./allNixFiles.nix { };
  allMachines = callPackage ./allMachines.nix { };
  makeNixOS = import ./makeNixOS.nix;
}
