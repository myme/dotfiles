# Sonnette

A simple notification bell for terminal workflows. Designed for tools like
Claude Code that run in tmux and need a way to signal when they're done.

## Usage

```
sonnette notify [title] [-- notify-send-args...]   # Ring the bell + desktop notification
sonnette status                                     # Show pending notifications (tmux styling)
sonnette jump                                       # Switch to the first alerting tmux pane
sonnette clear                                      # Clear all notifications
```

## How it works

Notifications are stored as marker files in `$XDG_RUNTIME_DIR/sonnette/`. When
running inside tmux, the pane ID (`$TMUX_PANE`) is used as the marker name,
enabling `sonnette jump` to switch directly to the alerting pane.

### Desktop notifications

`sonnette notify` sends a desktop notification via `notify-send`. The first
argument is the notification title. Context is automatically enriched:

- **TMUX session**: appended as `[session]` when running inside tmux
- **Project path**: appended from the JSON payload on stdin (`cwd` field)
- **Message**: extracted from stdin JSON (`message` field), falls back to
  "Waiting for input"

Extra arguments for `notify-send` can be passed after `--`:

```bash
sonnette notify "My Tool" -- --urgency=critical
sonnette notify "Build" -- -t 10000
```

Example notification title: `My Tool [main] - my-project`

## tmux integration

Add to your tmux status line:

```tmux
set -ag status-right "#(sonnette status)"
```

Bind keys for jumping and clearing:

```tmux
bind b run-shell 'sonnette jump'
bind B run-shell 'sonnette clear'
```

## Claude Code integration

Configure hooks in `~/.claude/settings.json`:

```json
{
  "hooks": {
    "Stop": [{ "hooks": [{ "type": "command", "command": "sonnette notify 'Claude Code'" }] }],
    "Notification": [{ "hooks": [{ "type": "command", "command": "sonnette notify 'Claude Code'" }] }]
  }
}
```

When Claude Code triggers a hook, it sends a JSON payload on stdin with fields
like `cwd` (project directory) and `message` (notification text). Sonnette
reads this to compose the desktop notification automatically.
