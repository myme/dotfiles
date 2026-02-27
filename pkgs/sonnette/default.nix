{ writeShellScriptBin, tmux, coreutils }:

writeShellScriptBin "sonnette" ''
  SONNETTE_DIR="''${SONNETTE_DIR:-''${XDG_RUNTIME_DIR:-/tmp}/sonnette}"

  notify() {
    ${coreutils}/bin/mkdir -p "$SONNETTE_DIR"
    local name="''${1:-''${TMUX_PANE:-$$}}"
    ${coreutils}/bin/touch "$SONNETTE_DIR/$name"
  }

  status() {
    if [ ! -d "$SONNETTE_DIR" ]; then
      return
    fi
    local count
    count=$(${coreutils}/bin/ls -1 "$SONNETTE_DIR" 2>/dev/null | ${coreutils}/bin/wc -l)
    if [ "$count" -gt 0 ]; then
      echo " 🔔 $count"
    fi
  }

  jump() {
    if [ ! -d "$SONNETTE_DIR" ]; then
      return 1
    fi
    local target
    target=$(${coreutils}/bin/ls -1 "$SONNETTE_DIR" 2>/dev/null | ${coreutils}/bin/head -1)
    if [ -z "$target" ]; then
      return 1
    fi
    # If it looks like a tmux pane ID, switch to it
    if [ -n "''${TMUX:-}" ] && [[ "$target" == %* ]]; then
      ${tmux}/bin/tmux switch-client -t "$target"
      ${coreutils}/bin/rm -f "$SONNETTE_DIR/$target"
    fi
  }

  clear() {
    ${coreutils}/bin/rm -rf "$SONNETTE_DIR"
  }

  case "''${1:-}" in
    notify) shift; notify "$@" ;;
    status) status ;;
    jump)   jump ;;
    clear)  clear ;;
    *)
      echo "Usage: sonnette {notify|status|jump|clear}" >&2
      exit 1
      ;;
  esac
''
