#!/bin/bash

WINDOW_MANAGER=i3

xcompmgr -n &

xrandr --output DP-1 --auto --primary
xrandr | grep "DVI-I-1 connected" | ifne xrandr --output DVI-I-1 --auto --right-of DP-1

# screen locking
xautolock -time 5 -locker "$HOME/.local/bin/i3-lock" &

# background image
nitrogen --restore &

# task bar apps
dropbox start &

exec $WINDOW_MANAGER
