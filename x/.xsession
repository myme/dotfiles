#!/bin/bash

WINDOW_MANAGER=i3
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

xcompmgr -n &

xrandr --output DP-1 --auto --primary
xrandr | grep "DVI-I-1 connected" | ifne xrandr --output DVI-I-1 --auto --right-of DP-1

nitrogen --restore &

# stalonetray &
dropbox start &
volumeicon &

exec $WINDOW_MANAGER
