{ config, lib, pkgs, specialArgs, ... }:

let
  machine = specialArgs.nixosConfig.myme.machine;
  isWsl = machine.flavor == "wsl";
  xauth = "${pkgs.xorg.xauth}/bin/xauth";
  kbdCfg = config.home.keyboard;
  cmdExe = "/mnt/c/Windows/System32/cmd.exe";

  # X commands
  stopx = "${cmdExe} /C taskkill /F /IM vcxsrv.exe";
  startx = pkgs.writeShellScriptBin "startx" ''
    # Start vcxsrv
    # See: https://sourceforge.net/p/vcxsrv/wiki/VcXsrv%20%26%20Win10/#windows-10-pro-version-20h2-setup-for-wsl2

    export PATH=$PATH:/run/current-system/sw/bin
    export DISPLAY="$(awk '/nameserver/ { print $2; }' /etc/resolv.conf):0.0";
    export WSLENV=1

    # Load ENV vars into systemd
    systemctl --user import-environment DISPLAY WSLENV

    # Stop possibly running server
    ${stopx}

    rm -vf ~/.Xauthority
    touch ~/.Xauthority
    magiccookie="$(cat /dev/urandom | head -c 256 | sha1sum | cut -d' ' -f1)"
    ${xauth} add "$DISPLAY" . "$magiccookie"

    # Copy .Xauthority to C:\Users\<user>
    user_profile="$(${cmdExe} /C "echo %USERPROFILE%" | tr -d '\r\n')"
    xauth_path="$(/bin/wslpath $user_profile)"
    cp ~/.Xauthority "$xauth_path"

    '/mnt/c/Program Files/VcXsrv/vcxsrv.exe' -multiwindow -clipboard -wgl -auth "$user_profile\.Xauthority" > ~/.VcXsrv.log 2>&1
  '';

  # Keyboard layout
  setxkbmap = pkgs.writeShellScriptBin "setxkbmap" ''
    export PATH=$PATH:/run/current-system/sw/bin
    export DISPLAY="$(awk '/nameserver/ { print $2; }' /etc/resolv.conf):0.0";
    ${pkgs.xorg.setxkbmap}/bin/setxkbmap ${kbdCfg.layout} -variant ${kbdCfg.variant};
  '';

in {
  config = lib.mkIf isWsl {
    home.sessionVariables = {
      # Set display variable for vcxsrv
      DISPLAY = "$(awk '/nameserver/ { print $2; }' /etc/resolv.conf):0.0";
    };

    # Install fonts
    myme.fonts.enable = true;

    # Start a 3rd party Xserver (VcXsrv)
    systemd.user.services.vcxsrv = {
      Unit = {
        Description = "VcXsrv";
        Documentation = "https://sourceforge.net/projects/vcxsrv/";
      };
      Service = {
        ExecStart = "${startx}/bin/startx";
        ExecStop = stopx;
        Type = "exec";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };

    # Set the preferred keyboard layout
    systemd.user.services.setxkbmap = {
      Unit = {
        Description = "setxkbmap";
        Requires = [ "vcxsrv.service" ];
      };
      Service = {
        ExecStart = "${setxkbmap}/bin/setxkbmap";
        Type = "oneshot";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
