{ lib, inputs, overlays }:
let
  allProfilesIf = import ./allProfilesIf.nix { inherit lib; };

in {
  myme = {
    allNixFiles = import ./allNixFiles.nix { inherit lib; };
    allProfiles = allProfilesIf (_: _: true);
    inherit allProfilesIf;
    nixos2hm = import ./nixos2hm.nix { inherit lib; };
    makeNixOS = import ./makeNixOS.nix { inherit inputs overlays; };
  };
}
