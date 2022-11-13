{ lib, inputs, overlays }:
{
  myme = {
    allNixFiles = import ./allNixFiles.nix { inherit lib; };
    allProfiles = import ./allProfiles.nix { inherit lib; };
    nixos2hm = import ./nixos2hm.nix { inherit lib; };
    makeNixOS = import ./makeNixOS.nix { inherit inputs overlays; };
  };
}
