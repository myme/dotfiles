{ home-manager, doomemacs, wallpapers }:

final: prev: {
  myme = {
    inherit doomemacs wallpapers;
    apps = builtins.listToAttrs (builtins.map (fname: {
      name = final.lib.strings.removeSuffix ".nix" fname;
      value = final.callPackage ./apps/${fname} { };
    }) (final.myme.lib.allNixFiles ./apps));
  };

  # Avoid nvidia vaapi driver collisions with e.g. intel
  # https://github.com/NixOS/nixpkgs/pull/165064
  nvidia-vaapi-driver = prev.lib.hiPrio prev.nvidia-vaapi-driver;
}
