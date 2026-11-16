{
  lib,
  hyprlock,
  hyprland,
}:

let
  # Hyprland >= 0.57 sends logically oriented screencopies. Hyprlock 0.9.7
  # rotates them again, making portrait displays look sideways while locked.
  # Backport https://github.com/hyprwm/hyprlock/pull/1094 (merged after 0.9.7).
  hasFix = builtins.any (
    patch:
    let
      name = baseNameOf "${patch}";
    in
    lib.hasInfix "bd846bae" name || name == "1094.patch"
  ) (hyprlock.patches or [ ]);
  cleanup = ''
    Remove pkgs/hyprlock/ and the programs.hyprlock.package assignment in
    home-manager/wm/hyprland/default.nix once nixpkgs includes the upstream fix.
  '';
in
if lib.versionOlder hyprland.version "0.57" then
  hyprlock
else
  assert lib.assertMsg (!hasFix) ''
    hyprlock: nixpkgs already backports the screenshot-rotation fix (upstream #1094).
    ${cleanup}
  '';
  assert lib.assertMsg (hyprlock.version == "0.9.7") ''
    hyprlock changed from the tested 0.9.7 to ${hyprlock.version}.
    Check whether the screenshot-rotation fix is now upstream before continuing.
    ${cleanup}
  '';
  hyprlock.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ./bd846bae16e8ea996f115cb04d33d55288aba808.patch ];
  })
