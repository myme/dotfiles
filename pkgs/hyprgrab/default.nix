# Hyprland screenshot script using `hyprshot` and `satty`

{ pkgs, ... }:

pkgs.writeShellScriptBin "hyprgrab" ''
  ${pkgs.hyprshot}/bin/hyprshot --mode ''${1:-region} --raw |
    ${pkgs.satty}/bin/satty \
      --filename - \
      --output-filename "$HOME/Pictures/Screenshots/%Y-%m-%d-%H%M%S.png"
''
