{-# LANGUAGE OverloadedStrings #-}
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
import qualified XMonad.StackSet                                                             as W

import           Control.Concurrent                       (threadDelay)
import           Data.Maybe                               (catMaybes)
import           Data.Monoid
import           Data.Ratio                               ((%))
import qualified DBus                                                                        as D
import qualified DBus.Client                                                                 as D
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           Numeric                                  (showHex)
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
                  (noTaskbar <+> doFloatAt 0.78 0.1)
              ,NS "charmap"
                  "gucharmap"
                  (appName =? "gucharmap")
                  (noTaskbar <+> doFloatPlace)
              ,NS "dict"
                  "mate-dictionary"
                  (appName =? "mate-dictionary")
                  (noTaskbar <+> doFloatPlace)
              ,NS "qterm"
                  "env _BSA_PSYS=qterm mate-terminal --disable-factory --hide-menubar --name=qterm"
                  (appName =? "qterm")
                  (customFloating (W.RationalRect 0.25 0 0.5 0.35))
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
                                onWorkspace "mail" qSimpleTabbed $
                                -- onWorkspace "calibre" Full $
                                onWorkspace "refs" qSimpleTabbed $
                                onWorkspace "emacs" revBasic -- ($)
                                basic
           ,manageHook        = composeAll
                                [appName =? "Pidgin" --> doShift "irc"
                                ,appName =? "xmessage" --> doFloatPlace
                                ,className =? "Trashapplet" --> doFloatPlace
                                ,className =? "Evolution-alarm-notify" --> doFloatPlace
                                ,className =? "Update-manager" --> doFloatPlace
                                ,className =? "Mate-dictionary" --> doFloatPlace
                                ,isInProperty "_NET_WM_STATE" "_NET_WM_STATE_STICKY" -->
                                 doF copyToAll <> doFloat
                                ,isInProperty "_NET_WM_STATE" "_NET_WM_STATE_ABOVE" -->
                                 doFloatPlace
                                ,appName =? "Pidgin" <&&> role =? "conversation" -->
                                 boing "phone-incoming-call"
                                -- this is a bit of a hack for the remote dcss scripts
                                ,appName =? "xfce4-terminal" <&&> role =? "dcss" -->
                                 doRectFloat (W.RationalRect 0.52 0.1 0.43 0.43)
                                ,manageSpawn
                                ,namedScratchpadManageHook scratchpads
                                ,placeHook myPlaceHook
                                ,isDialog --> doFloatPlace
                                ,manageHook baseConfig
                                ]
           ,logHook           = logTitle dbus <+>
                                logHook baseConfig <+>
                                setWorkArea -- @@@ HAAACK
           ,handleEventHook   = debuggering <+>
                                minimizeEventHook <+>
                                handleEventHook baseConfig
           ,startupHook       = do
             startupHook baseConfig
             doOnce $ do
               mateRegister
               spawn "exec compton -cCfGb --backend=glx"
               spawn "exec \"$HOME/.screenlayout/default.sh\""
               spawnOn "shell" "mate-terminal"
               spawnOn "emacs" "emacs"
               spawnOn "irc" "hexchat-utc"
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
            ,("M-C-S-m",    spawn "/opt/google/chrome/google-chrome --profile-directory=Default --app-id=ejhkdoiecgkmdpomoahkdihbcldkgjci")
             -- local crawl
            ,("M-C-c",      spawnAndDo doFloatPlace
                                       "xfce4-terminal --disable-server --working-directory=Sources/crawl/crawl-ref/source \
                                                     \ --title=DCSS --command=./crawl --geometry=81x25")
             -- crawl on underhound.eu
            ,("M-C-u",      spawnAndDo doFloatPlace
                                       "xfce4-terminal --disable-server \
                                                     \ --title=DCSS --command=cue --geometry=81x25")
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
            ,("M-C-S-6",    withFocused $ \w -> spawn $ "xprop -id 0x" ++ showHex w "" ++ " | xmessage -file -")
            ,("M-C-S-5",    withFocused $ \w -> spawn $ "xwininfo -id 0x" ++ showHex w "" ++ " -all | xmessage -file -")
            ,("M-b",        toggleBorders >> sendMessage ToggleStruts)
            ]
            ++
            -- greedyView -> view, so I stop breaking crawl etc. >.>
            [(otherModMasks ++ "M-" ++ [key], action tag)
             | (tag, key)              <- zip workspacen "1234567890-="
             , (otherModMasks, action) <- [("", windows . W.view) -- was W.greedyView
                                          ,("S-", windows . W.shift)
                                          ]
            ])

-- @@ ewmh copyTo: killAllOtherCopies >> windows (W.shift target), duh!

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
doFloatPlace = placeHook myPlaceHook <+> doFloat

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
pangoBold s = "<span weight=\"bold\" foreground=\"#ff0000\">" ++ unPango s ++ "</span>"

sounds :: String
sounds = "/usr/share/sounds/freedesktop/stereo"

boing :: String -> Query (Endo WindowSet)
boing sound = liftX (spawn $ "paplay " ++ sounds ++ "/" ++ sound ++ ".oga") >> idHook

debuggering :: Event -> X All
-- debuggering = debugEventsHook
debuggering = idHook

-- testing this

-- produce a RationalRect describing a window.
-- note that we don't use getWindowAttributes because it's broken...
-- @@@ is that still true?
getWinRR :: Window -> X (Maybe W.RationalRect)
getWinRR w = withDisplay $ \d -> do
  let fi :: Integral a => a -> Integer
      fi = fromIntegral
  wa' <- io $ alloca $ \wa'' -> do
    st <- xGetWindowAttributes d w wa''
    if st == 0
      then return Nothing
      else peek wa'' >>= return . Just
  case wa' of
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
    io $ changeProperty32 dpy r a c propModeReplace (concat $ replicate (length workspacen) [0, 26, 3840, 1028])
