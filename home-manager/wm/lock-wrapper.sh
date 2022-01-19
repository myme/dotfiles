#!/bin/sh

set -eu

# Stop compositor during lock due to issues with application content being
# painted over the lock screen: https://github.com/i3/i3lock/issues/204

systemctl --user stop picom.service
trap "systemctl --user start picom.service" EXIT

"$@"
