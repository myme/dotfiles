{ callPackage }:
{
  allNixFiles = callPackage ./allNixFiles.nix { };
  allProfiles = callPackage ./allProfiles.nix { };
  hm = callPackage ./hm.nix { };
  makeNixOS = import ./makeNixOS.nix;
}
