{ callPackage, home-manager }:
{
  allNixFiles = callPackage ./allNixFiles.nix { };
  allProfiles = callPackage ./allProfiles.nix { };
  nixos2hm = callPackage ./nixos2hm.nix { inherit home-manager; };
  makeNixOS = import ./makeNixOS.nix;
}
