# Hyprland screenshot script using `hyprshot` and `satty`

{ pkgs, ... }:

pkgs.writeShellScriptBin "hyprgrab" ''
  mode="''${1:-region}"
  if [ "$mode" = "output" ]; then
    mode="active --mode output"
  fi

  ${pkgs.hyprshot}/bin/hyprshot --mode $mode --raw |
    ${pkgs.satty}/bin/satty \
      --filename - \
      --output-filename "$HOME/Pictures/Screenshots/%Y-%m-%d-%H%M%S.png"
''
