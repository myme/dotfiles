# Hyprland screenshot script using `hyprshot` and `satty`

{ pkgs, ... }:

pkgs.writeShellScriptBin "hyprgrab" ''
  mode="''${1:-region}"
  output="$HOME/Pictures/Screenshots/$(date +%Y-%m-%d-%H%M%S).png"

  if [ "$mode" = "output" ]; then
    ${pkgs.hyprshot}/bin/hyprshot --mode output --output-folder "$(dirname "$output")" --filename "$(basename "$output")"
  else
    ${pkgs.hyprshot}/bin/hyprshot --mode "$mode" --raw |
      ${pkgs.satty}/bin/satty \
        --filename - \
        --output-filename "$output"
  fi
''
