{
  pkgs ? import <nixpkgs> {},
  lib ? pkgs.lib,
}:

pkgs.st.override {
  patches = [
    # ./st-alpha-0.8.2.diff
    # ./st-desktopentry-0.8.2.diff
    # ./st-selectioncolors-0.8.2.diff
    # (builtins.fetchurl {
    #   url = "https://st.suckless.org/patches/selectioncolors/st-selectioncolors-0.8.2.diff";
    #   sha256 = "195aicbwwl3iz71mi6gcknbgsymvd7vaz2fkia0bjw8a1bmwn7hn";
    # })
    (builtins.fetchurl {
      url = "https://st.suckless.org/patches/alpha/st-alpha-0.8.2.diff";
      sha256 = "11dj1z4llqbbki5cz1k1crr7ypnfqsfp7hsyr9wdx06y4d7lnnww";
    })
    (builtins.fetchurl {
      url = "https://st.suckless.org/patches/desktopentry/st-desktopentry-0.8.2.diff";
      sha256 = "0zl12xi8i10x3i2jy4lqg64vphkx77mjp7g1rc4kdd4q8saw7psx";
    })
  ];
  conf = lib.readFile ./config.def.h;
}
