#! /bin/sh
echo "Go update github xmonad and xmonad-contrib; I'll wait…"
read dummy
echo '$ cabal update'
cabal update
for pkg in X11 X11-xft xmonad xmonad-contrib; do
  echo '$ (cd "'"$pkg"'"; git pull)'
  (cd "$pkg"; git pull)
done
echo '$ cabal install xmonad --overwrite-policy-always'
cabal install xmonad --overwrite-policy=always
hash -r
echo '$ xmonad --recompile'
xmonad --recompile
echo '$ echo :q | cabal repl'
echo :q | cabal repl
# xmonad-log-applet original repo is gone; there's a new repo
# but it's fairly minimal-looking (but this might be a good
# thing as it might well be more portable). need to check this
# more closely. (upstream for the new repo also gone, so.)
