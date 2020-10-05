{
  pkgs ? import <nixpkgs> {},
  lib ? pkgs.lib,
}:

pkgs.st.override {
  patches = [
    (builtins.fetchurl {
      url = "https://st.suckless.org/patches/alpha/st-alpha-0.8.2.diff";
      sha256 = "11dj1z4llqbbki5cz1k1crr7ypnfqsfp7hsyr9wdx06y4d7lnnww";
    })
  ];
  conf = lib.readFile ./config.def.h;
}
