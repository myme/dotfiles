{
  config,
  lib,
  pkgs,
  ...
}:

let
  week = pkgs.writeShellScriptBin "week" ''
    grab() {
      filename=''${1:-"$(date +%F-%H%M%S).webm"}
      ${pkgs.wl-screenrec}/bin/wl-screenrec \
        --filename "$HOME/Videos/$filename" \
        --geometry "''$(${pkgs.slurp}/bin/slurp)"
    }

    action=''${1:-"grab"}
    case "$action" in
      grab)
        grab "$2"
        ;;
      status)
        if systemctl --user is-active week &>/dev/null; then
          echo '{"text": "■ stop", "class": "recording"}'
        else
          echo '{"text": "󰻃", "class": "idle"}'
        fi
        ;;
      start)
        systemctl --user start week
        ;;
      stop)
        systemctl --user stop week
        ;;
      toggle)
        if systemctl --user is-active week &>/dev/null; then
          systemctl --user stop week
        else
          systemctl --user start week
        fi
        ;;
    esac
  '';
in
{
  config = (
    lib.mkIf (config.myme.wm.enable && config.myme.wm.isWayland) {
      home.packages = [ week ];

      systemd.user.services.week = {
        Unit = {
          Description = "Week - A screen recording tool";
          # PartOf = ["graphical-session.target"];
        };
        # Install = {
        #   WantedBy = ["graphical-session.target"];
        # };
        Service = {
          Type = "simple";
          ExecStart = "${week}/bin/week";
        };
      };
    }
  );
}
