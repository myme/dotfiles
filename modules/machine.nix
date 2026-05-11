{ config, lib, ... }:

{
  options.myme.machine = {
    name = lib.mkOption {
      type = lib.types.str;
      default = "nixos";
      description = "Machine name";
    };
    stable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Use stable NixOS release";
    };
    role = lib.mkOption {
      type = lib.types.enum [ "desktop" "laptop" "server" ];
      default = "desktop";
      description = "Machine type";
    };
    flavor = lib.mkOption {
      type = lib.types.enum [ "nixos" "generic" "wsl" "darwin" ];
      default = "nixos";
      description = "OS flavor";
    };
    highDPI = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Optimize for high DPI outputs (4k)";
    };
    de.variant = lib.mkOption {
      type = lib.types.enum [
        "none"
        "hyprland"
        "gnome"
        "plasma"
        "wm"
        "xfce"
      ];
      default =
        if config.myme.machine.role == "server"
          || builtins.elem config.myme.machine.flavor [ "wsl" "darwin" ]
        then "none"
        else "wm";
      description = "Desktop Environment flavor";
    };
  };
}
