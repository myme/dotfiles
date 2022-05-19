{ lib, home-manager }:
{
  myme = {
    allNixFiles = import ./allNixFiles.nix { inherit lib; };
    allProfiles = import ./allProfiles.nix { inherit lib; };
    nixos2hm = import ./nixos2hm.nix { inherit lib home-manager; };
    makeNixOS = import ./makeNixOS.nix;
  };
}
