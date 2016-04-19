module XMonad.Util.NoTaskBar (-- * Usage
                              -- $usage
                              noTaskBar
                             ,markNoTaskBar) where

import XMonad.Core
import XMonad.ManageHook
import Graphics.X11.Xlib (Window)
import Graphics.X11.Xlib.Atom (aTOM)
import Graphics.X11.Xlib.Extras (getWindowProperty32
                                ,changeProperty32
                                ,propModePrepend)
import Control.Monad.Reader (ask)

-- $usage
-- Utility functions to hide windows from pagers and taskbars. Mostly useful
-- when EWMH doesn't do what you intend (e.g. for 'NamedScratchpad' windows you
-- probably don't want to be dumped into the 'NSP' workspace).

-- | A 'ManageHook' to mark a window to not be shown in pagers or taskbars.
noTaskBar :: ManageHook
noTaskBar = ask >>= (>> idHook) . liftX . markNoTaskBar

-- | An 'X' action to mark a window to not be shown in pagers or taskbars.
markNoTaskBar :: Window -> X ()
markNoTaskBar w = withDisplay $ \d -> do
                    ws <- getAtom "_NET_WM_STATE"
                    ntb <- getAtom "_NET_WM_STATE_SKIP_TASKBAR"
                    npg <- getAtom "_NET_WM_STATE_SKIP_PAGER"
                    wst' <- io $ getWindowProperty32 d ws w
                    io $ changeProperty32 d w ws aTOM propModePrepend [fi ntb,fi npg]

-- sigh
fi :: (Integral i, Num n) => i -> n
fi = fromIntegral
