{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.SpawnOn
import           XMonad.Config.Mate
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDebug
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.Place
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.IM
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.Simplest
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPane
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NoTaskbar
import           XMonad.Util.Run
import           XMonad.Util.SessionStart
import           XMonad.Util.Ungrab
import           XMonad.Util.WorkspaceCompare
import           XMonad.Prelude                           (fi, safeGetWindowAttributes, when)
import qualified XMonad.StackSet                                                             as W

import           Control.Concurrent                       (threadDelay)
import           Data.Maybe                               (catMaybes)
import           Data.Monoid
import           Data.Ratio                               ((%))
import           Data.Traversable
import qualified DBus                                                                        as D
import qualified DBus.Client                                                                 as D
import           System.IO                                (hPrint, hClose)
import           System.Posix.Env                         (putEnv)

-- sorry, I CBA to provide types for anything parameterized by layouts
baseConfig = debugManageHookOn "M-S-d" $
             ewmhFullscreen -- ($)
             mateConfig

workspacen :: [String]
workspacen =  ["shell", "emacs", "mail", "irc", "keep", "spare1",
               "windows", "crawl", "games", "spare2", "spare3", "spare4"]

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
               -- _BSA_PSYS: see ~/.prompt.pl (dotty repo, if you care)
              ,NS "qterm"
                  "env _BSA_PSYS=qterm mate-terminal --disable-factory --hide-menubar --name=qterm"
                  (appName =? "qterm")
                  (noTaskbar <> customFloating (W.RationalRect 0.25 0 0.5 0.35))
              ,NS "crawl-local"
                  "xfce4-terminal --disable-server --working-directory=Sources/crawl/crawl-ref/source \
                                \ --name crawl-local --title=DCSS --command=./crawl --geometry=81x25"
                  (appName =? "crawl-local")
                  (noTaskbar <> doFloatPlace)
               -- crawl on underhound.eu
              ,NS "crawl-cue"
                  "xfce4-terminal --disable-server \
                                \ --name crawl-cue --title=\"DCSS (CUE)\" --command=cue --geometry=81x25"
                  (appName =? "crawl-cue")
                  (noTaskbar <> doFloatPlace)
               -- crawl on crawl.kelbi.org
              ,NS "crawl-cko"
                  "xfce4-terminal --disable-server \
                                \ --name crawl-cko --title=\"DCSS (CKO)\" --command=cko --geometry=81x25"
                  (appName =? "crawl-cue")
                  (noTaskbar <> doFloatPlace)
               -- crawl on cbro.berotato.org
              ,NS "crawl-cbro"
                  "xfce4-terminal --disable-server \
                                \ --name crawl-cbro --title=\"DCSS (CBRO)\" --command=cbro --geometry=81x25"
                  (appName =? "crawl-cue")
                  (noTaskbar <> doFloatPlace)
               -- crawl on crawl.akrasiac.org
              ,NS "crawl-cao"
                  "xfce4-terminal --disable-server \
                                \ --name crawl-cao --title=\"DCSS (CAO)\" --command=cao --geometry=81x25"
                  (appName =? "crawl-cao")
                  (noTaskbar <> doFloatPlace)
               -- crawl on crawl.xtahua.com
              ,NS "crawl-cxc"
                  "xfce4-terminal --disable-server \
                                \ --name crawl-cxc --title=\"DCSS (CXC)\" --command=cxc --geometry=81x25"
                  (appName =? "crawl-cxc")
                  (noTaskbar <> doFloatPlace)
               -- crawl on crawl.develz.org
              ,NS "crawl-cdo"
                  "xfce4-terminal --disable-server \
                                \ --name crawl-cdo --title=\"DCSS (CDO)\" --command=cdo --geometry=81x25"
                  (appName =? "crawl-cxc")
                  (noTaskbar <> doFloatPlace)
              ]

main :: IO ()
main = do
  -- openjdk hackaround
  putEnv "_JAVA_AWT_WM_NONREPARENTING=1"  
  -- xmonad log applet
  dbus <- D.connectSession
  getWellKnownName dbus
  -- do it to it
  xmonad $ withUrgencyHook NoUrgencyHook baseConfig
           {modMask           = mod4Mask
           ,workspaces        = workspacen
           ,borderWidth       = 2
           ,focusedBorderColor= "#F57900" -- @@ from MATE theme
           ,normalBorderColor = "#444542" -- @@ from MATE theme, half bright
           ,focusFollowsMouse = False
           ,clickJustFocuses  = False
           ,layoutHook        = renamed [CutWordsLeft 2] $
                                minimize $
                                maximize $
                                lessBorders OnlyScreenFloat $
                                onWorkspace "windows" (avoidStrutsOn [] Full) $
                                avoidStruts $
                                onWorkspace "irc" (withIM 0.125 pidgin revBasic) $
                                onWorkspace "mail" revBasic $
                                -- onWorkspace "calibre" Full $
                                onWorkspace "refs" revBasic $
                                onWorkspace "spare2" emptyBSP $
                                onWorkspace "emacs" revBasic -- ($)
                                basic
           ,manageHook        = composeAll
                                [appName =? "Pidgin" --> doShift "irc"
                                ,appName =? "xmessage" --> doFloatPlace
                                ,className =? "Trashapplet" --> doFloatPlace
                                ,className =? "Evolution-alarm-notify" --> doFloatPlace
                                ,className =? "Update-manager" --> doFloatPlace
                                 -- I may change this back: on mod-q the focused window gets
                                 -- `copyToAll`-ed (but not floated). ???
                                ,isInProperty "_NET_WM_STATE" "_NET_WM_STATE_STICKY" -->
                                 doF copyToAll <> doFloatPlace
                                ,isInProperty "_NET_WM_STATE" "_NET_WM_STATE_ABOVE" -->
                                 doFloatPlace
                                ,appName =? "Pidgin" <&&> role =? "conversation" -->
                                 boing "phone-incoming-call"
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
                                handleEventHook baseConfig
           ,startupHook       = startupHook baseConfig <> doOnce do
                                  mateRegister
                                  spawn "exec compton -cCfGb --backend=glx"
                                  spawn "exec \"$HOME/.screenlayout/default.sh\""
                                  spawnOn "shell" "mate-terminal"
                                  -- if I have to restart xmonad because it crashed, these two will complain
                                  -- (hexchat's configured to regain my nick, so it'll get into fights if two
                                  -- are running; emacs complains about emacs-server and desktop file)
                                  -- (found by discovering xmonad-contrib#753)
                                  unlessQuery (appName =? "emacs") $ spawnOn "emacs" "emacs"
                                  unlessQuery (appName =? "hexchat") $ spawnOn "irc" "hexchat-utc"
                                  io $ threadDelay 1000000
                                  -- @@@ starts multi windows, placing them automatically will not fly :/
                                  spawnOn "mail" "google-chrome"
                                  setSessionStarted
           }
           `additionalKeysP`
           ([("M-C-g",      spawnHere "google-chrome")
            ,("M-C-e",      spawn "emacs")
            ,("M-C-v",      spawnOn "windows" "vmplayer")
            ,("M-C-s",      spawnHere "code")
             -- app.element.io
            ,("M-C-S-m",    spawn "/opt/google/chrome/google-chrome --profile-directory=Default \
                                                                  \ --app-id=ejhkdoiecgkmdpomoahkdihbcldkgjci")
            ,("M-C-cl",     namedScratchpadAction scratchpads "crawl-local")
            ,("M-C-cue",    namedScratchpadAction scratchpads "crawl-cue")
            ,("M-C-cko",    namedScratchpadAction scratchpads "crawl-cko")
            ,("M-C-cbr",    namedScratchpadAction scratchpads "crawl-cbro")
            ,("M-C-cao",    namedScratchpadAction scratchpads "crawl-cao")
            ,("M-C-cxc",    namedScratchpadAction scratchpads "crawl-cxc")
            ,("M-C-cdo",    namedScratchpadAction scratchpads "crawl-cdo")
            ,("M-C-k",      namedScratchpadAction scratchpads "calc")
            ,("M-C-m",      namedScratchpadAction scratchpads "charmap")
            ,("M-C-d",      namedScratchpadAction scratchpads "dict")
            ,("M-x",        namedScratchpadAction scratchpads "qterm")
            ,("M-<Right>",  moveTo Next hiddenWS)
            ,("M-<Left>",   moveTo Prev hiddenWS)
            ,("M-S-`",      withFocused $ sendMessage . maximizeRestore)
            ,("M-S-p",      mateRun)
            ,("M-p",        shellPrompt myXPConfig)
            ,("M-S-q",      mateShutdown)
             -- multiple-screen shot
            ,("M-S-s",      unGrab >> spawn "scrot -m ~/Downloads/screenshotM-%Y%m%dT%H%M%S.png")
             -- focused window shot
            ,("M-S-w",      unGrab >> spawn "scrot -u ~/Downloads/screenshotF-%Y%m%dT%H%M%S.png")
            -- ,("<Print>",    unGrab >> spawn "scrot -u ~/Downloads/screenshotF-%Y%m%dT%H%M%S.png")
            ,("<Print>",    unGrab >> spawn "xfce4-screenshooter")
             -- debug windows; also see M-S-d above
            ,("M-C-S-8",    withFocused showWinRR)
            ,("M-C-S-7",    spawn "xprop | xmessage -file -")
            ,("M-C-S-6",    withFocused $ \w -> spawn $ "xprop -id " ++ show w ++ " | xmessage -file -")
            ,("M-C-S-5",    withFocused $ \w -> spawn $ "xwininfo -id " ++ show w ++ " -all | xmessage -file -")
            ,("M-b",        toggleBorders >> sendMessage ToggleStruts)
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
            [(otherModMasks ++ "M-" ++ [key], action tag)
             | (tag, key)              <- zip workspacen "1234567890-="
             , (otherModMasks, action) <- [("", windows . W.view) -- was W.greedyView
                                          ,("S-", windows . W.shift)
                                          ]
            ])

-- @@@ not quite right... refresh? (hacked above)
-- @@@@ and cast still doesn't dtrt. possibly chrome's fail
toggleBorders :: X ()
toggleBorders = withFocused $ \w -> do
                  d <- asks display
                  bw <- asks $ borderWidth . config
                  wa <- io $ getWindowAttributes d w
                  let nbw = if wa_border_width wa == 0 then bw else 0
                  io $ setWindowBorderWidth d w nbw

myPlaceHook :: Placement
myPlaceHook = inBounds $ smart (0.5, 0.5)

doFloatPlace :: ManageHook
doFloatPlace = placeHook myPlaceHook <> doFloat

pidgin :: Property
pidgin = Resource "Pidgin" `And` Role "buddy_list"

basic = TwoPane 0.03 0.5 ||| qSimpleTabbed ||| Simplest
revBasic = qSimpleTabbed ||| TwoPane 0.03 0.5 ||| Simplest
qSimpleTabbed = renamed [CutWordsRight 1] $
                tabbed shrinkText def {fontName = "xft:Mono-8"}

myXPConfig :: XPConfig
myXPConfig = greenXPConfig {promptKeymap = emacsLikeXPKeymap
                           ,font = "xft:Mono-9"
                           }

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

logTitle :: D.Client -> X ()
logTitle ch = dynamicLogWithPP def
                               {ppCurrent = unPango
                               ,ppVisible = pangoInactive
                               ,ppHidden  = const ""
                               ,ppHiddenNoWindows = const ""
                               ,ppUrgent  = pangoBold
                               ,ppTitle   = unPango
                               ,ppLayout  = unPango
                               ,ppWsSep   = " "
                               ,ppSep     = "⋮"
                               ,ppOrder   = swapIcons
                               ,ppSort    = getSortByXineramaPhysicalRule
                                              horizontalScreenOrderer
                               ,ppOutput  = dbusOutput ch
                               }
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
  let sig = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update")
            {D.signalBody = [D.toVariant s]}
  D.emit ch sig

-- quick and dirty escaping of HTMLish Pango markup
unPango :: String -> String
unPango []       = []
unPango ('<':xs) = "&lt;" ++ unPango xs
unPango ('&':xs) = "&amp;" ++ unPango xs
unPango ('>':xs) = "&gt;" ++ unPango xs
unPango (x  :xs) = x:unPango xs

-- show a string as inactive
-- @@@ should use gtk theme somehow...
pangoInactive :: String -> String
pangoInactive s = "<span foreground=\"#8f8f8f\">" ++ unPango s ++ "</span>"

-- show a string with highlight
pangoBold :: String -> String
pangoBold s = "<span weight=\"bold\" foreground=\"#ff2f2f\">" ++ unPango s ++ "</span>"

sounds :: String
sounds = "/usr/share/sounds/freedesktop/stereo"

boing :: String -> Query (Endo WindowSet)
boing sound = liftX (spawn $ "paplay " ++ sounds ++ "/" ++ sound ++ ".oga") >> idHook

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
  p <- spawnPipe "xmessage -file -"
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
  rs <- for ws $ runQuery q
  when (null rs || not (or rs)) x