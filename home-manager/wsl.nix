{
  config,
  lib,
  pkgs,
  specialArgs,
  ...
}:

let
  machine = specialArgs.nixosConfig.myme.machine;
  isWsl = machine.flavor == "wsl";

in
{
  config = lib.mkIf isWsl {
    home.sessionVariables = {
      BROWSER = "${pkgs.writeShellScript "win-browser" ''
        exec '/mnt/c/Program Files/Mozilla Firefox/firefox.exe' "$@"
      ''}";
    };

    # Install fonts
    myme.fonts.enable = true;

    # Force "true-color" for Helix on WSL
    programs.helix.settings.editor.true-color = true;
  };
}
