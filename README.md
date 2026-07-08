xmonad config on my current laptop.
none of these are "master", I change things on one and consider
cherrypicking to the other depending on the nature of the change.

picom.conf should be symlinked into ~/.config.
10profile should be symlinked into /etc/X11/xinit.d on Debianoids.
if using geekosaur/dotty in some fashion, bsa-common.com should be
(renamed and) adjusted as needed and symlinked into
~/.config/environment.d on Fedoroids. (Ideally it would source
your replacement for ~/.bsa-common (see geekosaur/dotty), but
systemd doesn't permit that.)

this config uses cabal to build. it assumes that X11, X11-xft,
xmonad, xmonad-contrib, and xmonad-extras have been checked out
into this directory; change cabal.project if you are getting them
from hackage or have them checked out elsewhere, or want to remove
xmonad-extras (don't forget to update the cabal file in that case!).
