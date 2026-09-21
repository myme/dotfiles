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
    logfile="''${XDG_RUNTIME_DIR:-/tmp}/week.log"

    notify() { "$notify" -a week "week" "$1" 2>/dev/null || true; }

    is_recording() { [ -f "$pidfile" ] && kill -0 "$(cat "$pidfile")" 2>/dev/null; }

    start() {
      if is_recording; then
        notify "Already recording"
        exit 0
      fi
      target="''${1:-region}"
      mkdir -p "$video_dir"
      # H.264/mp4, not VP9/webm: wl-screenrec picks the codec from the
      # container extension and encodes on the GPU, and hardly any VAAPI
      # driver exposes a VP9 *encode* entrypoint. AMD's VCN (radeonsi) decodes
      # VP9 but cannot encode it, so .webm died on the spot with "No usable
      # encoding entrypoint found for profile VAProfileVP9Profile0". H.264 is
      # the one profile every GPU this runs on can encode. To check on a new
      # machine, run `vainfo`: the codec needs VAEntrypointEncSlice listed,
      # not just VAEntrypointVLD.
      filename="$video_dir/$(date +%F-%H%M%S).mp4"
      case "$target" in
        region)
          geometry="$($slurp)" || exit 1
          set -- --geometry "$geometry"
          ;;
        window)
          geometry="$($hyprctl activewindow -j | $jq -r '"\(.at[0]),\(.at[1]) \(.size[0])x\(.size[1])"')"
          set -- --geometry "$geometry"
          ;;
        monitor | output)
          output="$($slurp -o -f '%o')" || exit 1
          set -- --output "$output"
          ;;
        *)
          echo "week: unknown target '$target' (region|window|monitor)" >&2
          exit 1
          ;;
      esac
      "$setsid" "$recorder" --filename "$filename" "$@" >"$logfile" 2>&1 &
      pid=$!
      echo "$pid" > "$pidfile"
      # wl-screenrec fails fast on an unsupported codec or a bad geometry.
      # Without this check the notification claims success, the recording is
      # simply absent, and the reason went to /dev/null -- so confirm the
      # process survived and otherwise show what it said.
      sleep 0.5
      if ! kill -0 "$pid" 2>/dev/null; then
        rm -f "$pidfile"
        notify "Recording failed: $(tail -n1 "$logfile")"
        exit 1
      fi
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
