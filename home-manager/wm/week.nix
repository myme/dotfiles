{
  config,
  lib,
  pkgs,
  ...
}:

let
  # Screen recording helper around wl-screenrec. Supports recording an
  # arbitrary region, the active window, or a whole monitor. Recording state
  # is tracked via a PID file so start/stop/toggle work from anywhere (a
  # keybind, or the DMS "capture" bar plugin) without a systemd service.
  week = pkgs.writeShellScriptBin "week" ''
    set -u

    slurp="${pkgs.slurp}/bin/slurp"
    recorder="${pkgs.wl-screenrec}/bin/wl-screenrec"
    hyprctl="${pkgs.hyprland}/bin/hyprctl"
    jq="${pkgs.jq}/bin/jq"
    setsid="${pkgs.util-linux}/bin/setsid"
    notify="${pkgs.libnotify}/bin/notify-send"

    video_dir="$HOME/Videos"
    pidfile="''${XDG_RUNTIME_DIR:-/tmp}/week.pid"

    notify() { "$notify" -a week "week" "$1" 2>/dev/null || true; }

    is_recording() { [ -f "$pidfile" ] && kill -0 "$(cat "$pidfile")" 2>/dev/null; }

    start() {
      if is_recording; then
        notify "Already recording"
        exit 0
      fi
      target="''${1:-region}"
      mkdir -p "$video_dir"
      filename="$video_dir/$(date +%F-%H%M%S).webm"
      case "$target" in
        region)
          geometry="$($slurp)" || exit 1
          "$setsid" "$recorder" --filename "$filename" --geometry "$geometry" >/dev/null 2>&1 &
          ;;
        window)
          geometry="$($hyprctl activewindow -j | $jq -r '"\(.at[0]),\(.at[1]) \(.size[0])x\(.size[1])"')"
          "$setsid" "$recorder" --filename "$filename" --geometry "$geometry" >/dev/null 2>&1 &
          ;;
        monitor | output)
          output="$($slurp -o -f '%o')" || exit 1
          "$setsid" "$recorder" --filename "$filename" --output "$output" >/dev/null 2>&1 &
          ;;
        *)
          echo "week: unknown target '$target' (region|window|monitor)" >&2
          exit 1
          ;;
      esac
      echo $! > "$pidfile"
      notify "Recording ($target) → $filename"
    }

    stop() {
      if ! is_recording; then
        rm -f "$pidfile"
        notify "Not recording"
        exit 0
      fi
      # wl-screenrec finalizes the file cleanly on SIGINT.
      kill -INT "$(cat "$pidfile")" 2>/dev/null || true
      rm -f "$pidfile"
      notify "Recording saved to $video_dir"
    }

    action="''${1:-toggle}"
    case "$action" in
      start) start "''${2:-region}" ;;
      stop) stop ;;
      grab) start "''${2:-region}" ;;
      toggle)
        if is_recording; then stop; else start "''${2:-region}"; fi
        ;;
      status)
        if is_recording; then
          echo '{"text": "■ stop", "class": "recording"}'
        else
          echo '{"text": "󰻃", "class": "idle"}'
        fi
        ;;
      *)
        echo "usage: week {toggle|start|stop|status} [region|window|monitor]" >&2
        exit 1
        ;;
    esac
  '';
in
{
  config = lib.mkIf (config.myme.wm.enable && config.myme.wm.isWayland) {
    home.packages = [ week ];
  };
}
