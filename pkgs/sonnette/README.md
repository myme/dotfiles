# Sonnette

A simple notification bell for terminal workflows. Designed for tools like
Claude Code that run in tmux and need a way to signal when they're done.

## Usage

```
sonnette notify [name]   # Ring the bell (name defaults to $TMUX_PANE or PID)
sonnette status          # Show pending notifications (with tmux styling)
sonnette jump            # Switch to the first alerting tmux pane and clear it
sonnette clear           # Clear all notifications
```

## How it works

Notifications are stored as marker files in `$XDG_RUNTIME_DIR/sonnette/`. When
running inside tmux, the pane ID (`$TMUX_PANE`) is used as the marker name,
enabling `sonnette jump` to switch directly to the alerting pane.

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
    "Stop": [{ "hooks": [{ "type": "command", "command": "sonnette notify" }] }],
    "Notification": [{ "hooks": [{ "type": "command", "command": "sonnette notify" }] }]
  }
}
```
