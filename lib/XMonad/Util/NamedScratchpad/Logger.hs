module XMonad.Util.NamedScratchpad.Logger (nspTrackStartup
                                          ,nspTrackHook
                                          ,nspActiveIcon
                                          ,nspActive
                                          ,nspActive') where

import XMonad.Core
import Graphics.X11.Xlib (Window)
import Graphics.X11.Xlib.Extras (Event(..))
import XMonad.Util.NamedScratchpad (NamedScratchpad(..))
import qualified XMonad.Util.ExtensibleState as XS
import Data.Monoid (All(..))
import Data.Char (chr)
import Control.Monad (forM, foldM)
import qualified Data.IntMap as M
import qualified XMonad.StackSet as W (allWindows)

data NSPTrack = NSPTrack [Maybe Window] deriving Typeable
instance ExtensionClass NSPTrack where
  initialValue = NSPTrack []

-- startupHook to initialize scratchpad activation tracking
-- , startupHook = ... <+> nspTrackStartup scratchpads
-- (if you kickstart the logHook, do it *after* nspTrackStartup!)
nspTrackStartup :: [NamedScratchpad] -> X ()
nspTrackStartup ns = do
  let ns'i = M.fromList $ zip [0..] $ map (const Nothing) ns
  ns' <- withWindowSet $ foldM (isSp ns) ns'i . W.allWindows
  XS.put (NSPTrack (map snd $ M.toAscList ns'))

isSp :: [NamedScratchpad] -> M.IntMap (Maybe Window) -> Window -> X (M.IntMap (Maybe Window))
isSp ns ws w = do
  n <- runQuery (scratchpadWindow ns) w
  return $ case n of
            Nothing -> ws
            Just n' -> M.insert n' (Just w) ws

scratchpadWindow :: [NamedScratchpad] -> Query (Maybe Int)
scratchpadWindow ns = foldM sp' Nothing (zip [0..] ns)
  where sp' :: Maybe Int -> (Int,NamedScratchpad) -> Query (Maybe Int)
        sp' r@(Just _) _              = return r
        sp' Nothing    (n,NS _ _ q _) = q >>= \p -> return $ if p then Just n else Nothing

-- handleEventHook to track scratchpad activation/deactivation
-- , handleEventHook = ... <+> nspTrackHook scratchpads
nspTrackHook :: [NamedScratchpad] -> Event -> X All
nspTrackHook _ (DestroyWindowEvent {ev_window = w}) = do
  XS.modify $ \(NSPTrack ws) -> NSPTrack $ map (\sw -> if sw == Just w then Nothing else sw) ws
  return (All True)
nspTrackHook ns (ConfigureRequestEvent {ev_window = w}) = do
  NSPTrack ws <- XS.get
  ws' <- forM (zip3 [0..] ws ns) $ \(n,w',NS _ _ q _) -> do
    p <- runQuery q w
    return $ if p then Just w else w'
  XS.put $ NSPTrack ws'
  return (All True)
nspTrackHook _ _ = return (All True)

-- Logger (see XMonad.Util.Loggers and XMonad.Hooks.DynamicLog PP {ppExtras})
--   for scratchpads' state
-- , ppExtras = [..., nspActive' iconChars showActive showInactive, ...]
nspActiveIcon :: [Char] -> (String -> String) -> (String -> String) -> X (Maybe String)
nspActiveIcon icns act inact = do
  NSPTrack ws <- XS.get
  return $ if null ws
            then Nothing
            else let icon' n = if n < length icns then icns !! n else '\NUL'
                     icon  n = let c = icon' n
                                in [if c == '\NUL' then chr (0x2460 + n) else c]
                     ckact n w = let icn = icon n
                                  in case w of
                                      Nothing -> inact icn
                                      Just _  -> act   icn
                     s = unwords $ zipWith ckact [0..] ws
                  in Just s

-- Logger with String-s (and no defaults)
-- , ppExtras = [..., nspActive iconStrs showActive showInactive, ...]
nspActive :: [String] -> (String -> String) -> (String -> String) -> X (Maybe String)
nspActive icns act inact = do
  NSPTrack ws <- XS.get
  return $ if null ws
            then Nothing
            else let  ckact n w = let icn = icns !! n
                                    in case w of
                                        Nothing -> inact icn
                                        Just _  -> act   icn
                      s = unwords $ zipWith ckact [0..] ws
                  in Just s

-- Variant of the above getting the String-s from the NamedScratchpad-s
nspActive' :: [NamedScratchpad] -> (String -> String) -> (String -> String) -> X (Maybe String)
nspActive' ns = nspActive (map name ns)
