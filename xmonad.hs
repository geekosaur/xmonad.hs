{-# LANGUAGE OverloadedStrings #-} -- dbus insists on it. and it appears to be partial :(
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Redundant id" #-}

import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.TiledWindowDragging
import           XMonad.Actions.SpawnOn
import           XMonad.Config.Mate
import           XMonad.Hooks.StatusBar.PP
-- import           XMonad.Hooks.DebugEvents
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDebug
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.Place
import           XMonad.Hooks.Rescreen
import           XMonad.Hooks.ScreenCorners
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.DraggingVisualizer
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPane
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NoTaskbar
import           XMonad.Util.Run
import           XMonad.Util.SessionStart
import           XMonad.Util.WorkspaceCompare

import           XMonad.Prelude                           (fi
                                                          ,safeGetWindowAttributes
                                                          ,when
                                                          ,toUpper
                                                          ,findM
                                                          ,void)
import qualified XMonad.StackSet                                                              as W

import           Control.Concurrent                       (threadDelay)
import           Control.Exception                        (handle
                                                          ,IOException)
import           Data.Maybe                               (catMaybes
                                                          ,isNothing)
import           Data.Monoid
import           Data.Ratio                               ((%))
import qualified DBus                                                                         as D
import qualified DBus.Client                                                                  as D
import           System.IO                                (hPrint
                                                          ,hClose)
import           System.Posix.IO

-- sorry, I CBA to provide types for anything parameterized by layouts
baseConfig = debugManageHookOn "M-S-d" $
             ewmhFullscreen $
             setEwmhActivateHook doAskUrgent -- ($)
             mateConfig

shellWs, emacsWs, mailWs, chatWs, refsWs, devWs :: String
winWs, crawlWs, gamesWs, spareWs, booksWs, botsWs :: String
-- names subject to change next time I restart my session
shellWs = "shell"
emacsWs = "emacs"
mailWs = "mail"
chatWs = "irc"
refsWs = "keep"
devWs = "dev"
winWs = "windows"
crawlWs = "crawl"
gamesWs = "games"
spareWs = "spare"
booksWs = "calibre"
botsWs = "xmonadtrack"

workspacen :: [String]
workspacen =  [shellWs, emacsWs, mailWs, chatWs, refsWs, devWs,
               winWs, crawlWs, gamesWs, spareWs, booksWs, botsWs,
               -- these 4 are for testing
               "spare1", "spare2", "spare3", "spare4"]

-- helper for crawl named scratchpads
remoteCrawl :: [Char] -> NamedScratchpad
remoteCrawl svr = NS ("crawl-" ++ svr)
                     ("xfce4-terminal --disable-server --role crawl-" ++ svr ++
                      " --title=\"DCSS (" ++ map toUpper svr ++ ")\" --command=" ++
                      svr ++ " --geometry=80x25")
                     (role =? ("crawl-" ++ svr))
                     (noTaskbar <> doFloatPlace)

scratchpads :: [NamedScratchpad]
scratchpads = [NS "calc"
                  -- @@@ perhaps assign a specific name or role for this
                  "mate-calc"
                  (appName =? "mate-calc")
                  (noTaskbar <> doFloatAt 0.78 0.1)
              ,NS "charmap"
                  "gucharmap"
                  (appName =? "gucharmap")
                  (noTaskbar <> doFloatPlace)
              ,NS "dict"
                  "mate-dictionary"
                  (appName =? "mate-dictionary")
                  (noTaskbar <> doFloatPlace)
               -- _BSA_PSYS: see ~/.prompt.pl and ~/.zshenv (dotty repo, if you care)
              ,NS "qterm"
                  "env _BSA_PSYS=qterm mate-terminal --disable-factory --hide-menubar --name=qterm"
                  (appName =? "qterm")
                  (noTaskbar <> customFloating (W.RationalRect 0.25 0 0.5 0.35))
              ,NS "crawl-local"
                  "xfce4-terminal --disable-server --working-directory=Sources/crawl/crawl-ref/source \
                                \ --role crawl-local --title=DCSS --command=./crawl --geometry=81x25"
                  (role =? "crawl-local")
                  (noTaskbar <> doFloatPlace)
              ,remoteCrawl "cue" -- underhound.eu
              ,remoteCrawl "cbro" -- berotato.org
              ,remoteCrawl "cao" -- akrasiac.org
              ,remoteCrawl "cxc" -- xtahua.com
              ,remoteCrawl "cdo" -- develz.org
              ,remoteCrawl "cdi" -- dcss.io
               -- if you're wondering, the missing ones are:
               --   cpo (Australia, and tiles-only)
               --   cnc (Korean)
               --   lld (Japanese)
              ,NS "uclock"
                  "dclock -name uclock -miltime -utc -fg chartreuse -bg DarkSlateGrey -led_off DarkGreen"
                  (appName =? "uclock")
                  (noTaskbar <> doFloatAt (1730/1920) (6/1080))
              ,NS "lclock"
                  "dclock -name lclock -miltime -fade -fg yellow -bg sienna4 -led_off DarkGoldenrod4"
                  (appName =? "lclock")
                  (noTaskbar <> doFloatAt (1730/1920) (6/1080))
              ]

main :: IO ()
main = do
  -- gdm is logging to syslog, which is also being mangled by some amdgpu
  -- misconfiguration I'm still trying to sort out
  void $ handle (\(_ :: IOException) -> return ()) $ closeFd stdInput
  void $ openFd "/dev/null" ReadOnly Nothing defaultFileFlags
  void $ handle (\(_ :: IOException) -> return ()) $ closeFd stdOutput
  void $ openFd "/home/allbery/.cache/xmonad/xmonad.log" ReadWrite (Just 0o644) defaultFileFlags {trunc = True}
  void $ handle (\(_ :: IOException) -> return ()) $ closeFd stdError
  void $ dupTo stdOutput stdError
  -- xmonad log applet
  dbus <- D.connectSession
  getWellKnownName dbus
  -- do it to it
  xmonad $ withUrgencyHook NoUrgencyHook
         $ rescreenHook (RescreenConfig reApplyARandR reApplyARandR)
         $ baseConfig
           {modMask           = mod4Mask
           ,workspaces        = workspacen
           ,borderWidth       = 2
           ,focusedBorderColor= "#F57900" -- @@ from MATE theme
           ,normalBorderColor = "#444542" -- @@ from MATE theme
           ,focusFollowsMouse = False
           ,clickJustFocuses  = False
           ,terminal          = terminal baseConfig ++ " --disable-factory"
           ,layoutHook        = screenCornerLayoutHook $
                                renamed [CutWordsLeft 3] $
                                draggingVisualizer $
                                minimize $
                                maximize $
                                lessBorders OnlyScreenFloat $
                                onWorkspace winWs (avoidStrutsOn [] Full) $
                                avoidStruts $
                                onWorkspace chatWs basic1 $
                                onWorkspace mailWs basic2 $
                                onWorkspace booksWs Full $
                                onWorkspace refsWs basic2 $
                                onWorkspace spareWs emptyBSP $
                                onWorkspace emacsWs basic2 $
                                id basic -- shut up hlint (I append layout modifiers for testing a lot)
           ,manageHook        = composeAll
                                [appName =? "xmessage" --> doCenterFloat
                                ,appName =? "zenity" --> doCenterFloat
                                ,className =? "Trashapplet" --> doFloatPlace
                                ,className =? "Evolution-alarm-notify" --> doFloatPlace
                                ,className =? "Update-manager" --> doFloatPlace
                                -- needed until and unless the new startNheko works
                                ,className =? "nheko" --> doShift chatWs
                                ,appName =? "sxiv" --> noTaskbar <> doShift spareWs
                                ,isInProperty "_NET_WM_STATE" "_NET_WM_STATE_ABOVE" --> doFloatPlace
                                ,manageSpawn
                                ,namedScratchpadManageHook scratchpads
                                ,placeHook myPlaceHook
                                ,isDialog --> doFloatPlace
                                ,manageHook baseConfig
                                ]
           ,logHook           = logTitle dbus <>
                                logHook baseConfig <>
                                setWorkArea -- @@@ HAAACK
           ,handleEventHook   = debuggering <>
                                minimizeEventHook <>
                                screenCornerEventHook <>
                                notificationEventHook <>
                                handleEventHook baseConfig
           ,startupHook       = startupHook baseConfig <>
                                addScreenCorner SCUpperRight (spawn "mate-screensaver-command --activate") <>
                                doOnce do
                                  mateRegister
                                  spawn "exec picom -cfb --backend=glx"
                                  reApplyARandR
                                  asks (terminal . config) >>= spawnOn shellWs
                                  asks (terminal . config) >>= spawnOn shellWs
                                  -- if I have to restart xmonad because it crashed, these two will complain
                                  -- (hexchat's configured to regain my nick, so it'll get into fights if two
                                  -- are running; emacs complains about emacs-server and desktop file)
                                  -- (found by discovering xmonad-contrib#753)
                                  -- unlessQuery (appName =? "emacs") $ spawnOn emacsWs "emacs"
                                  unlessQuery (appName =? "sublime_text") $ spawnOn emacsWs "subl"
                                  unlessQuery (appName =? "io.github.NhekoReborn.Nheko") startNheko
                                  io $ threadDelay 3000000
                                  unlessQuery (appName =? "hexchat") $ spawnOn chatWs "hexchat-utc"
                                  io $ threadDelay 3000000
                                  -- @@@ starts multi windows, placing them automatically will not fly :/
                                  unlessQuery (appName =? "google-chrome") $ spawnOn mailWs "google-chrome --force-device-scale-factor=1.0"
                                  setSessionStarted
           }
           `additionalKeysP`
           [("M-C-g",             spawnHere "google-chrome --force-device-scale-factor=1.0")
           ,("M-C-S-g",           spawnHere "firefox")
           ,("M-C-e",             spawnHere "emacsclient -c")
           ,("M-C-S-e",           spawnOn emacsWs "emacs")
           ,("M-C-n",             startNheko)
           ,("M-C-v",             spawnOn winWs "vmplayer")
           ,("M-C-s",             spawnOn devWs "codium")
             -- app.element.io
           ,("M-C-S-n",           spawn "/opt/google/chrome/google-chrome --profile-directory=Default \
                                                                        \ --force-device-scale-factor=1.0 \
                                                                        \ --app-id=ejhkdoiecgkmdpomoahkdihbcldkgjci")
           ,("M-C-c l",           namedScratchpadAction scratchpads "crawl-local")
           ,("M-C-c u e",         namedScratchpadAction scratchpads "crawl-cue")
           ,("M-C-c k o",         namedScratchpadAction scratchpads "crawl-cko")
           ,("M-C-c b r",         namedScratchpadAction scratchpads "crawl-cbro")
           ,("M-C-c a o",         namedScratchpadAction scratchpads "crawl-cao")
           ,("M-C-c x c",         namedScratchpadAction scratchpads "crawl-cxc")
           ,("M-C-c d o",         namedScratchpadAction scratchpads "crawl-cdo")
           ,("M-C-c d i",         namedScratchpadAction scratchpads "crawl-cdi")
           ,("M-C-k",             namedScratchpadAction scratchpads "calc")
           ,("M-C-m",             namedScratchpadAction scratchpads "charmap")
           ,("M-C-d",             namedScratchpadAction scratchpads "dict")
           ,("M-x",               namedScratchpadAction scratchpads "qterm")
           ,("M-C-u",             namedScratchpadAction scratchpads "uclock")
           ,("M-C-l",             namedScratchpadAction scratchpads "lclock")
           ,("M-<Right>",         moveTo Next hiddenWS)
           ,("M-<Left>",          moveTo Prev hiddenWS)
           ,("M-C-`",             withFocused $ sendMessage . maximizeRestore)
           ,("M-S-p",             mateRun)
           ,("M-p",               shellPrompt myXPConfig)
           ,("M-S-q",             mateShutdown)
           ,("M-C-S-q",           mateLogout)
           ,("M-C-S-u",           spawn "update-manager")
           ,("M-C-S-s",           spawn "mate-control-center")
           ,("<Print>",           unGrab >> spawn "xfce4-screenshooter")
             -- debug windows; also see M-S-d above
           ,("M-C-S-w r",         withFocused showWinRR)
           ,("M-C-S-w p",         spawn "xprop | ${XMONAD_XMESSAGE:-xmessage} -file -")
           ,("M-C-S-w f",         withFocused $ \w -> spawn $ "xprop -id " ++ show w ++ " | ${XMONAD_XMESSAGE:-xmessage} -file -")
           ,("M-C-S-w i",         withFocused $ \w -> spawn $ "xwininfo -id " ++ show w ++ " -all | ${XMONAD_XMESSAGE:-xmessage} -file -")
           ,("M-b",               toggleBorders >> sendMessage ToggleStruts)
             -- BSP actions
           ,("M-C-S-p <Left>",    sendMessage $ ExpandTowards L)
           ,("M-C-S-p <Right>",   sendMessage $ ShrinkFrom L)
           ,("M-C-S-p <Up>",      sendMessage $ ExpandTowards U)
           ,("M-C-S-p <Down>",    sendMessage $ ShrinkFrom U)
           ,("M-C-S-p C-<Left>",  sendMessage $ ShrinkFrom R)
           ,("M-C-S-p C-<Right>", sendMessage $ ExpandTowards R)
           ,("M-C-S-p C-<Up>",    sendMessage $ ShrinkFrom D)
           ,("M-C-S-p C-<Down>",  sendMessage $ ExpandTowards D)
           ,("M-C-S-p s",         sendMessage   Swap)
           ,("M-C-S-p r",         sendMessage   Rotate)
           ,("M-C-S-p b",         sendMessage   Balance)
           ,("M-C-S-p e",         sendMessage   Equalize)
           ,("M-C-S-p j",         sendMessage $ SplitShift Prev)
           ,("M-C-S-p k",         sendMessage $ SplitShift Next)
           ]
           ++
           -- greedyView -> view, so I stop breaking crawl etc. >.>
           [ (otherModMasks ++ "M-" ++ key, action tag)
           | (tag, key) <- zip workspacen (words "1 2 3 4 5 6 7 8 9 0 - = <F1> <F2> <F3> <F4>")
           , (otherModMasks, action) <- [("", windows . W.view) -- was W.greedyView
                                        ,("S-", windows . W.shift)
                                        ]
           ]
           `additionalMouseBindings`
           [((mod4Mask .|. controlMask, button1), dragWindow)
           ]

-- @@@ not quite right... refresh? (hacked above)
-- @@@@ and cast still doesn't dtrt. possibly chrome's fail
toggleBorders :: X ()
toggleBorders = withFocused $ \w -> do
                  d <- asks display
                  bw <- asks $ borderWidth . config
                  wa <- io $ getWindowAttributes d w
                  let nbw = if wa_border_width wa == 0 then bw else 0
                  io $ setWindowBorderWidth d w nbw

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

myPlaceHook :: Placement
myPlaceHook = inBounds $ smart (0.5, 0.5)

doFloatPlace :: ManageHook
doFloatPlace = placeHook myPlaceHook <> doFloat

-- we really need a way to preset a starting point in the layout rotation…
basic = TwoPane 0.03 0.5 ||| Mirror (TwoPane 0.03 0.5) ||| qSimpleTabbed
basic1 = Mirror (TwoPane 0.03 0.5) ||| qSimpleTabbed ||| TwoPane 0.03 0.5
basic2 = qSimpleTabbed ||| TwoPane 0.03 0.5 ||| Mirror (TwoPane 0.03 0.5)

-- note on the font here and in `myXPConfig`: I've got a QHD laptop. had to
-- replace the external monitor (formerly fullHD) with QHD to get any semblance
-- of sanity from X11, and it's still not very; I need very small font sizes
-- (and other UI elements when possible) to avoid everything being in "easy
-- reader" mode.
--
-- suffice it to say I'm now one of those waiting on xmonad-for-wayland…
qSimpleTabbed = renamed [CutWordsRight 1] $
                tabbed shrinkText def {fontName = "xft:Mono-6"}
 
sounds :: String
sounds = "/usr/share/sounds/freedesktop/stereo"

-- boing :: String -> Query (Endo WindowSet)
-- boing sound = liftX (boing' sound) >> idHook

boing' :: String -> X ()
boing' sound = spawn $ "paplay " ++ sounds ++ "/" ++ sound ++ ".oga"

startNheko :: X ()
startNheko =
  -- spawnOn won't work unless the pid is exposed, but I have low confidence in that version
  -- XDG_CURRENT_DESKTOP works around a crash on right-click
  -- spawn "flatpak run --env=TZ=UTC0 io.github.NhekoReborn.Nheko"
  spawn "flatpak run --env=TZ=UTC0 --env=XDG_CURRENT_DESKTOP= --env=QT_SCALE_FACTOR=1.5 im.nheko.Nheko"
  -- getProcessId >>= \p -> spawnOn chatWs ("flatpak run --env=TZ=UTC0 --parent-expose-pids --parent-pid=" ++
  --                                       show p ++ " io.github.NhekoReborn.Nheko")

reApplyARandR :: X ()
reApplyARandR = spawn "exec \"$HOME/.screenlayout/default.sh\"" -- whoops: worked exactly once
-- reApplyARandR = return () -- apparently MATE finally fixed mate-display-properties!

-- this needs to be cleaned up
notificationEventHook :: Event -> X All
notificationEventHook MapNotifyEvent {ev_window = w} = do
  -- try to identify notification windows
  nw <- withDisplay $ \d -> getStringProperty d w "WM_CLASS"
  case nw of
    Nothing -> return ()
    -- don't ask me why the one notification you'd expect to be quiet is the
    -- only one that's reasonably audible…
    Just s -> when (s == "mate-notification-daemon\NULMate-notification-daemon\NUL") $
                boing' "onboard-key-feedback"
  return (All True)
notificationEventHook _ = return (All True)

myXPConfig :: XPConfig
myXPConfig = greenXPConfig {promptKeymap = emacsLikeXPKeymap
                           ,font         = "xft:Mono-6"
                           }

logTitle :: D.Client -> X ()
logTitle ch = dynamicLogWithPP def
                               {ppCurrent         = unPango
                               ,ppVisible         = pangoInactive
                               ,ppHidden          = const ""
                               ,ppHiddenNoWindows = const ""
                               ,ppUrgent          = pangoBold
                               ,ppTitle           = unPango
                               ,ppLayout          = unPango
                               ,ppWsSep           = " "
                               ,ppSep             = " ⋮ "
                               ,ppOrder           = swapIcons
                               ,ppSort            = getSortByXineramaPhysicalRule horizontalScreenOrderer
                               ,ppOutput          = dbusOutput ch
                               }
        -- not currently using this, but left for testing etc.
  where swapIcons (ws:l:t:nsp:xs) = ws:l:nsp:t:xs
        -- @@@ so why do the first 4 invocations *only* not match?!
        swapIcons xs              = xs

getWellKnownName :: D.Client -> IO ()
getWellKnownName ch = do
  _ <- D.requestName ch
                     (D.busName_ "org.xmonad.Log")
                     [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput ch s = do
  let sig = (D.signal "/org/xmonad/Log"
                      "org.xmonad.Log"
                      "Update")
            {D.signalBody = [D.toVariant s]}
  D.emit ch sig

-- quick and dirty escaping of HTMLish Pango markup
unPango :: String -> String
unPango []       = []
unPango ('<':xs) = "&lt;"  ++ unPango xs
unPango ('&':xs) = "&amp;" ++ unPango xs
unPango ('>':xs) = "&gt;"  ++ unPango xs
unPango (x  :xs) = x:unPango xs

-- show a string as inactive
-- @@@ should use gtk theme somehow...
pangoInactive :: String -> String
pangoInactive s = "<span foreground=\"#8f8f8f\">" ++ unPango s ++ "</span>"

-- show a string with highlight
pangoBold :: String -> String
pangoBold s = "<span weight=\"bold\" foreground=\"#ff2f2f\">" ++ unPango s ++ "</span>"

debuggering :: Event -> X All
-- debuggering = debugEventsHook
debuggering = idHook

-- produce a RationalRect describing a window
getWinRR :: Window -> X (Maybe W.RationalRect)
getWinRR w = do
  safeGetWindowAttributes w >>= \case
    Nothing -> return Nothing
    Just wa -> do
      dis <- gets $ W.screens . windowset
      -- need to know which screen
      let rs = flip map dis $ \di ->
                let Rectangle rx' ry' rw' rh' = screenRect $ W.screenDetail di
                    rx = fi rx'
                    ry = fi ry'
                    rw = fi rw'
                    rh = fi rh'
                    wx = fi $ wa_x wa
                    wy = fi $ wa_y wa
                    ww = fi (wa_width  wa) + 2 * fi (wa_border_width wa)
                    wh = fi (wa_height wa) + 2 * fi (wa_border_width wa)
                in if wx >= rx && wx <= rx + rw && wy >= ry && wy < ry + rh
                  then Just $ W.RationalRect ((wx - rx) % rw) ((wy - ry) % rh) (ww % rw) (wh % rh)
                  else Nothing
      return $ case catMaybes rs of
                (r:_) -> Just r
                _     -> Nothing

showWinRR :: Window -> X ()
showWinRR w = do
  p <- spawnPipe "${XMONAD_XMESSAGE:-xmessage} -file -"
  getWinRR w >>= io . hPrint p
  io $ hClose p

-- @@@@@@@@ HAAAAAAAAAACK
setWorkArea :: X ()
setWorkArea = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WORKAREA"
    c <- getAtom "CARDINAL"
    r <- asks theRoot
    io $ changeProperty32 dpy r a c propModeReplace
                          (concat $ replicate (length workspacen) [0, 26, 3840, 1028])

-- run an action only if no windows match a Query Bool.
-- this is moderately expensive, but sometimes duplicating an action
-- is more so…
unlessQuery :: Query Bool -> X () -> X ()
unlessQuery q x = do
  d <- asks display
  r <- asks theRoot
  (_,_,ws) <- io $ queryTree d r
  -- [Leary] is offended by my quick and dirty code :p
  match <- findM (runQuery q) ws
  when (isNothing match) x
