#!/bin/bash
count=$(xrandr --listactivemonitors | sed '1{;s/Monitors: //;q;}')
if [ "$count" -eq 1 ]; then
    xrandr --output eDP --primary --mode 2256x1504 --pos 0x0 --rotate normal --output DisplayPort-0 --off --output DisplayPort-1 --off --output DisplayPort-2 --off --output DisplayPort-3 --off --output DisplayPort-4 --off --output DisplayPort-5 --off --output DisplayPort-6 --off --output DisplayPort-7 --off
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

panels="$(xrandr | sed -e 1d -e '/^[^ ]/!d' -e '/ disconnected /d' -e 's/ .*$//' -e '/^eDP$/d' -e '/^DisplayPort-2$/d' | tr '\n' ' ')"
panels="${panels/ *}" # strictly speaking there should be only one left
dps="$(xrandr | sed -e 1d -e '/^[^ ]/!d' -e 's/ .*$//' -e '/^eDP$/d' | tr '\n' ' ')"

randr=
for output in $dps; do
    case " $panels " in
    *" $output "*)
        randr="$randr --output $output --right-of eDP"
        ;;
    *)
        randr="$randr --output $output --off"
        ;;
    esac
done

# note for if xrandr screws up again and drops the whole thing to 1080p:
# eDP: --mode 2256x1504
# DisplayPort-2: --mode 2560x1440
# <random>: --mode 2560x1440

# shellcheck disable=SC2086
xrandr --output eDP --primary --output DisplayPort-2 --left-of eDP $randr
