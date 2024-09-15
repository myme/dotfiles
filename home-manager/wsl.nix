{ config, lib, specialArgs, ... }:

let
  machine = specialArgs.nixosConfig.myme.machine;
  isWsl = machine.flavor == "wsl";

in {
  config = lib.mkIf isWsl {
    # Install fonts
    myme.fonts.enable = true;

    # Force "true-color" for Helix on WSL
    programs.helix.settings.editor.true-color = true;
  };
}
