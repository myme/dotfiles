{ config, lib, pkgs, specialArgs, ... }:

let
  machine = specialArgs.nixosConfig.myme.machine;
  isWsl = machine.flavor == "wsl";
  xauth = "${pkgs.xorg.xauth}/bin/xauth";
  kbdCfg = config.home.keyboard;
  startx = pkgs.writeShellScriptBin "startx" ''
    # Start vcxsrv
    # See: https://sourceforge.net/p/vcxsrv/wiki/VcXsrv%20%26%20Win10/#windows-10-pro-version-20h2-setup-for-wsl2

    rm -vf ~/.Xauthority
    touch ~/.Xauthority
    magiccookie="$(cat /dev/urandom | head -c 256 | sha1sum | cut -d' ' -f1)"
    ${xauth} add "$DISPLAY" . "$magiccookie"

    # Copy .Xauthority to C:\Users\<user>
    user_profile="$(/mnt/c/Windows/System32/cmd.exe /C "echo %USERPROFILE%" | tr -d '\r\n')"
    cp ~/.Xauthority "$(wslpath $user_profile)"

    '/mnt/c/Program Files/VcXsrv/vcxsrv.exe' -multiwindow -clipboard -wgl -auth "$user_profile\.Xauthority" > ~/.VcXsrv.log 2>&1 &
    disown
  '';
  init = pkgs.writeShellScriptBin "init-wsl" ''
    # Load ENV vars into systemd
    systemctl --user import-environment DISPLAY SSH_AUTH_SOCK XDG_DATA_DIRS XDG_RUNTIME_DIR WSLENV
    ${startx}/bin/startx
    ${pkgs.xorg.setxkbmap}/bin/setxkbmap ${kbdCfg.layout} -variant ${kbdCfg.variant}
  '';

in {
  config = lib.mkIf isWsl {
    home.sessionVariables = {
      # Set display variable for vcxsrv
      DISPLAY = "$(awk '/nameserver/ { print $2; }' /etc/resolv.conf):0.0";
    };

    home.packages = [
      init
      startx
    ];

    # TODO: Tweak this systemd unit. The `startx` script ran with issues.
    # systemd.user.services.vcxsrv = {
    #   Unit = {
    #     Description = "VcXsrv";
    #     Documentation = "https://sourceforge.net/projects/vcxsrv/";
    #   };
    #   Service = {
    #     ExecStart = "${startx}";
    #     Type = "oneshot";
    #   };
    #   Install = {
    #     WantedBy = [ "default.target" ];
    #   };
    # };
  };
}
