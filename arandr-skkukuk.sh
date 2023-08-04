#!/bin/sh
if xrandr --listactivemonitors 2>&1 | grep 'Monitors: 2'; then
  xrandr --output LVDS-1 --primary --mode 1366x768 --pos 1920x0 --rotate normal \
  	 --output VGA-1 --mode 1920x1080 --pos 0x0 --rotate normal \
	 --output HDMI-1 --off --output DP-1 --off \
	 --output HDMI-2 --off --output HDMI-3 --off \
	 --output DP-2 --off --output DP-3 --off
else
  xrandr --output LVDS-1 --primary --mode 1366x768 --pos 0x0 --rotate normal \
	 --output VGA-1 --off \
	 --output HDMI-1 --off --output DP-1 --off \
	 --output HDMI-2 --off --output HDMI-3 --off \
	 --output DP-2 --off --output DP-3 --off
fi
