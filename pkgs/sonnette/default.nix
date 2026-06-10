{
  writeShellScriptBin,
  tmux,
  coreutils,
  jq,
  libnotify,
}:

writeShellScriptBin "sonnette" ''
  SONNETTE_DIR="''${SONNETTE_DIR:-''${XDG_RUNTIME_DIR:-/tmp}/sonnette}"

  notify() {
    ${coreutils}/bin/mkdir -p "$SONNETTE_DIR"
    local name="''${TMUX_PANE:-$$}"
    ${coreutils}/bin/touch "$SONNETTE_DIR/$name"

    # Parse args: [title] [-- notify-send-args...]
    local title=""
    local ns_args=()
    while [ $# -gt 0 ]; do
      if [ "$1" = "--" ]; then
        shift
        ns_args=("$@")
        break
      fi
      title="$1"
      shift
    done

    # Read JSON from stdin if not a terminal (e.g. hook payload)
    local input=""
    if [ ! -t 0 ]; then
      input=$(${coreutils}/bin/cat)
    fi

    # Extract context from JSON
    local cwd="" message=""
    if [ -n "$input" ]; then
      cwd=$(echo "$input" | ${jq}/bin/jq -r '.cwd // empty' 2>/dev/null)
      message=$(echo "$input" | ${jq}/bin/jq -r '.message // empty' 2>/dev/null)
    fi

    # Enrich title with tmux session and project path
    if [ -n "''${TMUX:-}" ]; then
      local session
      session=$(${tmux}/bin/tmux display-message -p '#S' 2>/dev/null)
      if [ -n "$session" ]; then
        title="''${title:+$title }[$session]"
      fi
    fi
    if [ -n "$cwd" ]; then
      title="''${title:+$title - }$(${coreutils}/bin/basename "$cwd")"
    fi
    title="''${title:-Notification}"

    # Build notification body
    local body="''${message:-Ready}"

    # Send desktop notification
    ${libnotify}/bin/notify-send "$title" "$body" "''${ns_args[@]}"
  }

  status() {
    if [ ! -d "$SONNETTE_DIR" ]; then
      return
    fi
    local count
    count=$(${coreutils}/bin/ls -1 "$SONNETTE_DIR" 2>/dev/null | ${coreutils}/bin/wc -l)
    if [ "$count" -gt 0 ]; then
      echo " 🔔 $count "
    fi
  }

  jump() {
    if [ ! -d "$SONNETTE_DIR" ]; then
      return
    fi
    local target
    target=$(${coreutils}/bin/ls -1 "$SONNETTE_DIR" 2>/dev/null | ${coreutils}/bin/head -1)
    if [ -z "$target" ]; then
      return
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
