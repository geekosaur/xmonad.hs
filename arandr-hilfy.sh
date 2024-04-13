#!/bin/sh
count=$(xrandr --listactivemonitors | sed '1{;s/Monitors: //;q;}')
if [ "$count" -eq 1 ]; then
    xrandr --output eDP --primary --mode 2256x1504 --pos 0x0 --rotate normal --output DisplayPort-0 --off --output DisplayPort-1 --off --output DisplayPort-2 --off --output DisplayPort-3 --off --output DisplayPort-4 --off --output DisplayPort-5 --off --output DisplayPort-6 --off --output DisplayPort-7 --off
else
    xrandr --output eDP --primary --mode 2256x1504 --pos 2560x0 --rotate normal --output DisplayPort-0 --off --output DisplayPort-1 --off --output DisplayPort-2 --mode 2560x1440 --pos 0x0 --rotate normal --output DisplayPort-3 --off --output DisplayPort-4 --off --output DisplayPort-5 --off --output DisplayPort-6 --off --output DisplayPort-7 --off
fi
