# See: https://github.com/nix-community/NixOS-WSL/issues/235#issuecomment-2259058000
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
{

  options.fix.docker-desktop.enable = mkEnableOption "docker desktop fix";

  config =
    let
      resources = "C:\\Program Files\\Docker\\Docker\\resources";
    in
    mkIf (config.wsl.docker-desktop.enable && config.fix.docker-desktop.enable) {
      systemd.services.docker-desktop-proxy = {
        script = mkForce ''
          ${config.wsl.wslConf.automount.root}/wsl/docker-desktop/docker-desktop-user-distro proxy --docker-desktop-root ${config.wsl.wslConf.automount.root}/wsl/docker-desktop '${resources}'
        '';
        path = [ pkgs.mount ];
      };
    };
}
