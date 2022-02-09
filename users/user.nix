{
  users.users.user = {
    isNormalUser = true;
    initialPassword = "nixos";
    extraGroups = [ "wheel" "networkmanager" ];
    openssh.authorizedKeys.keys = [];
  };

  home-manager.users.user = {
    imports = [
      ../home-manager
    ];

    config = {
      myme.alacritty.font_size = 10;
      myme.wm = {
        enable = true;
        variant = "xmonad";
        # variant = "i3";
        conky = false;
        polybar.monitor = "Virtual-1";
      };
    };
  };
}
