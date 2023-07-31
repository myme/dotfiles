{ lib, home-manager, doomemacs, wallpapers }:

final: prev: {
  agenix = prev.agenix.override {
    # `age` works better than `rage` for editing .age files with SSH keys with
    # passphrases as of 2023-02-16.
    ageBin = "${prev.age}/bin/age";
  };

  gnupg240 = let
    pname = "gnupg";
    version = "2.4.0";
  in prev.gnupg.overrideAttrs {
    inherit pname version;
    src = prev.fetchurl {
      url = "mirror://gnupg/gnupg/${pname}-${version}.tar.bz2";
      hash = "sha256-HXkVjdAdmSQx3S4/rLif2slxJ/iXhOosthDGAPsMFIM=";
    };
  };

  myme = {
    inherit doomemacs wallpapers;
    pkgs = builtins.listToAttrs (builtins.map (fname: {
      name = final.lib.strings.removeSuffix ".nix" fname;
      value = final.callPackage ./pkgs/${fname} { };
    }) (lib.myme.allNixFiles ./pkgs));
  };

  # Avoid nvidia vaapi driver collisions with e.g. intel
  # https://github.com/NixOS/nixpkgs/pull/165064
  nvidia-vaapi-driver = prev.lib.hiPrio prev.nvidia-vaapi-driver;
}
