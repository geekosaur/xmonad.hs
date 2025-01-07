#!/bin/bash
exec >>/tmp/screenlayout.dbg 2>&1
printf "--------\n"
count=$(xrandr --listactivemonitors | sed '1{;s/Monitors: //;q;}')
if [ "$count" -eq 1 ]; then
    xrandr --output eDP --primary --mode 2880x1920 --pos 0x0 --rotate normal --output DisplayPort-0 --off --output DisplayPort-1 --off --output DisplayPort-2 --off --output DisplayPort-3 --off --output DisplayPort-4 --off --output DisplayPort-5 --off --output DisplayPort-6 --off --output DisplayPort-7 --off
    exit 0
fi
# hack: the monitor on the right turns off its input on suspend,
# # which triggers this script, which wakes everything back up.
if [ "$count" -eq 2 ] && [ "$1" \!= "force" ]; then
    echo screensaver detected
    exit 0
fi

# The outputs move around to some extent.
# eDP is the laptop panel
# DisplayPort-2 is the left external monitor
# There are two others, because of the hardware hack needed for the other
# external monitor to run at full resolution (otherwise it runs at 1080p):
# it's connected via both DisplayPort and HDMI. We want the former, and
# want to turn off the latter. The DisplayPort is always first.
# (Note that all the non-built-in displays are always exposed to X11 as if
# they are DisplayPort, but are actually USB3 on the laptop end.)
# Note that one of the outputs may be DisplayPort-0 or DisplayPort-1!
#
# We also need to extract the preferred mode for each display, because
# they all get downgraded to match the stupidity of the above-mentioned
# 1080p-default monitor. So we need to extract the mode of the one marked
# with `+`. (But see the later comment.)
#
# NOTE: At some point the DisplayPort port on the second external monitor
# did whatever DisplayPorts always do for me (hardware failure), so that
# is no longer connected. The monitor still starts at 1080p, but can be
# upgraded successfully now without the DP being connected. (Don't ask me.)
#

panels="$(xrandr | sed -e 1d -e '/^[^ ]/!d' -e '/ disconnected /d' -e 's/ .*$//' -e '/^eDP$/d' -e '/^DisplayPort-2$/d' | tr '\n' ' ')"
#panels="${panels/ *}" # strictly speaking there should be only one left
dps="$(xrandr | sed -e 1d -e '/^[^ ]/!d' -e 's/ .*$//' -e '/^eDP$/d' | tr '\n' ' ')"

randr=
for output in $dps; do
    case " $panels " in
	*" $output "*)
        randr="$randr --output $output --right-of eDP --mode 2560x1440"
        ;;
    *)
        randr="$randr --output $output --off"
        ;;
    esac
done

# note for if xrandr screws up again and drops the whole thing to 1080p:
# eDP: --mode 2880x1920
# DisplayPort-2: --mode 2560x1440
# <random>: --mode 2560x1440

xrandr
doit="xrandr --output eDP --primary --mode 2880x1920 --output DisplayPort-2 --left-of eDP --mode 2560x1440 $randr"
echo "$doit"
eval "$doit"
