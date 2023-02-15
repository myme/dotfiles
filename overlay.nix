{ lib, home-manager, doomemacs, wallpapers }:

final: prev: {
  agenix = prev.agenix.override {
    # `age` works better than `rage` for editing .age files with SSH keys with
    # passphrases as of 2023-02-16.
    ageBin = "${prev.age}/bin/age";
  };

  myme = {
    inherit doomemacs wallpapers;
    apps = builtins.listToAttrs (builtins.map (fname: {
      name = final.lib.strings.removeSuffix ".nix" fname;
      value = final.callPackage ./apps/${fname} { };
    }) (lib.myme.allNixFiles ./apps));
  };

  # Avoid nvidia vaapi driver collisions with e.g. intel
  # https://github.com/NixOS/nixpkgs/pull/165064
  nvidia-vaapi-driver = prev.lib.hiPrio prev.nvidia-vaapi-driver;
}
