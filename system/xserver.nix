{ pkgs, ... }:
{
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "alt-intl-unicode";

  # Enable touchpad support
  services.xserver.libinput.enable = true;
  services.xserver.libinput.touchpad.naturalScrolling = true;

  # Home manager xsession
  services.xserver.desktopManager.session = [
    {
      name = "home-manager";
      start = ''
        ${pkgs.runtimeShell} $HOME/.hm-xsession &
        waitPID=$!
      '';
    }
  ];
}
