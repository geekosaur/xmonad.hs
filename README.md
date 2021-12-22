xmonad config on my current laptop.
neither of these is "master", I change things on one and consider
cherrypicking to the other depending on the nature of the change.

compton.conf should be symlinked into ~/.config.

this config uses cabal to build. it assumes that X11, X11-xft,
xmonad, and xmonad-contrib have been checked out into this
directory; change cabal.project if you are getting them from
hackage or have them checked out elsewhere.
