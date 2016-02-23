{-# LANGUAGE OverloadedStrings #-}

import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.SpawnOn
import           XMonad.Config.Mate
-- import           XMonad.Hooks.DebugEvents
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDebug
import           XMonad.Hooks.ManageDocks
-- import           XMonad.Hooks.HackDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.Place
import           XMonad.Layout.Accordion
import           XMonad.Layout.GridVariants
import           XMonad.Layout.IM
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.OneBig
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.StackTile
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPane
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Ungrab
import           XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet                                                             as W

import qualified Codec.Binary.UTF8.String                                                    as UTF8
import           Control.Monad
import           Data.Monoid
import qualified DBus                                                                        as D
import qualified DBus.Client                                                                 as D
import           System.Environment                       (getArgs)
import           System.Posix.Env                         (putEnv)
import           System.Posix.IO
import           Control.Concurrent                       (threadDelay)
import           Numeric                                  (showHex)
import           System.IO                                (hPutStrLn, hClose)
import           XMonad.Util.Run

import           Graphics.X11.Xlib.Extras
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           Data.Maybe                               (catMaybes)
import           Data.Ratio                               ((%))

baseConfig = debugManageHookOn "M-S-d" mateConfig

scratchpads = [NS "notes1"
                  "leafpad --name=notes1 ~/Documents/Notepad.txt"
                  (appName =? "notes1")
                  (setUtility <+> noTaskBar <+> customFloating (W.RationalRect 0.4 0.35 0.2 0.3))
              ,NS "timelog"
                  "leafpad --name=timelog ~/Documents/Timelog.txt"
                  (appName =? "timelog")
                  (setUtility <+> noTaskBar <+> customFloating (W.RationalRect 0.1 0.1 0.2 0.3))
              ,NS "notes3"
                  "leafpad --name=notes3 ~/Documents/Notepad3.txt"
                  (appName =? "notes3")
                  (setUtility <+> noTaskBar <+> customFloating (W.RationalRect 0.7 0.1 0.2 0.3))
              ,NS "notes4"
                  "leafpad --name=notes4 ~/Documents/Notepad4.txt"
                  (appName =? "notes4")
                  (setUtility <+> noTaskBar <+> customFloating (W.RationalRect 0.1 0.7 0.2 0.3))
              ,NS "notes5"
                  "leafpad --name=notes5 ~/Documents/Notepad5.txt"
                  (appName =? "notes5")
                  (setUtility <+> noTaskBar <+> customFloating (W.RationalRect 0.7 0.7 0.2 0.3))
              ,NS "calc"
                  -- @@@ perhaps assign a specific name or role for this
                  "mate-calc"
                  (appName =? "mate-calc")
                  (setUtility <+> noTaskBar <+> doFloatAt 0.78 0.1)
              ,NS "charmap"
                  "gucharmap"
                  (appName =? "gucharmap")
                  (setUtility <+> noTaskBar <+> doFloatPlace)
              ,NS "qterm"
                  "mate-terminal --disable-factory --hide-menubar --name=qterm"
                  (appName =? "qterm")
                  (customFloating (W.RationalRect 0.275 0 0.45 0.3))
              ,NS "crawl"
                  "xfce4-terminal --disable-server --hide-toolbar --hide-menubar --role=crawl --title=Crawl -e $HOME/.bin/crawloop"
                  (appName =? "xfce4-terminal" <&&> role =? "crawl")
                  -- @@@ fails, terminal bug? 23x79
                  -- @@@@ or does the core not account for the border, which is *inside* the window?
                  -- (doFloatAt 0.52 0.1)
                  -- @@@ BEWARE this is jiggered to be 80x24
                  -- @@@@ and rejiggering confirms: it's the border, and issue is in
                  --      X.O.windows since default width isn't accounting for border
                  (customFloating (W.RationalRect 0.52 0.1 (735/1920) (462/1080)))
              ,NS "mtr"
                  "mate-terminal --disable-factory --hide-menubar --name=mtr --title=mtr -x sudo mtr --curses 198.58.116.136"
                  (appName =? "mtr")
                  (setUtility <+> noTaskBar <+> customFloating (W.RationalRect 0 0 1 0.55))
              ]

workspacen :: [String]
workspacen =  ["emacs", "irc", "mail", "chrome", "openafs", "pending", "win10", "games", "tv", "calibre", "misc", "spare"]

main = do
  -- something is undoing this. regularly.
  -- make shift-space = space
  spawn "xmodmap -e 'keycode 65 = space space space space NoSymbol NoSymbol thinspace nobreakspace'"
  -- make a second middle button on my mouse since the scrollwheel's fiddly/oversensitive
  -- @@@@ also 11, since it seems they swap sometimes?!
  spawn "xinput --set-button-map  9 1 2 3 4 5 6 7 8 9 10 11 12 13; \
        \xinput --set-button-map  9 1 2 3 4 5 6 7 8 2 10 11 12 13; \
        \xinput --set-button-map 11 1 2 3 4 5 6 7 8 9 10 11 12 13; \
        \xinput --set-button-map 11 1 2 3 4 5 6 7 8 2 10 11 12 13"
  -- openjdk hackaround
  putEnv "_JAVA_AWT_WM_NONREPARENTING=1"
  -- xmonad log applet
  dbus <- D.connectSession
  getWellKnownName dbus
  -- do it to it
  -- @@ see https://github.com/xmonad/xmonad/commit/307b82a53d519f5c86c009eb1a54044a616e4a5c
  as <- getArgs
  xmonad $ baseConfig
           {modMask           = mod4Mask
           ,workspaces        = workspacen
           ,borderWidth       = 2
           ,focusedBorderColor= "#F57900" -- @@ from MATE theme
           ,normalBorderColor = "#444542" -- @@ from MATE theme, half bright
           ,focusFollowsMouse = False
           ,clickJustFocuses  = False
            -- @@ renames are out of sync now
           ,layoutHook        = renamed [CutWordsLeft 2] $
                                minimize $
                                maximize $
                                lessBorders OnlyFloat $
                                onWorkspace "win10" (avoidStrutsOn [] Full) $
                                avoidStruts $
                                onWorkspace "irc" (withIM 0.125 pidgin ims) $
                                onWorkspace "calibre" Full $
                                onWorkspace "games" Full $
                                onWorkspace "mail" simpleTabbed $
                                onWorkspace "openafs" simpleTabbed $
                                TwoPane 0.03 0.5 |||
                                simpleTabbed |||
                                Full
           ,manageHook        = composeAll
                                [appName =? "Pidgin" --> doShift "irc"
                                ,appName =? "xmessage" --> doFloatPlace
                                ,appName =? "trashapplet" --> doFloatPlace
                                -- @@@ copyToAll, but need a window parameter...
                                ,isInProperty "_NET_WM_STATE" "_NET_WM_STATE_STICKY" --> doIgnore
                                -- ,appName =? "Pidgin" <&&> role =? "conversation" --> boing "phone-incoming-call"
                                -- this is a bit of a hack for the remote dcss scripts
                                ,appName =? "xfce4-terminal" <&&> role =? "dcss" --> doRectFloat (W.RationalRect 0.52 0.1 0.43 0.43)
                                ,manageSpawn
                                ,namedScratchpadManageHook scratchpads
                                ,placeHook myPlaceHook
                                ,isDialog --> doFloatPlace
                                ,manageHook baseConfig
                                ]
           ,logHook           = logTitle dbus <+> logHook baseConfig
           ,handleEventHook   = debuggering <+>
                                fullscreenEventHook <+>
                                minimizeEventHook <+>
                                handleEventHook baseConfig
           ,startupHook       = do
             startupHook baseConfig
             when (null as) $ do
               spawn "compton -cCfGb --backend=glx"
               io $ threadDelay 2500000
               -- @@@ starts multi windows, placing them automatically will not fly :/
               -- spawnOn "mail" spawnChrome
               -- spawnOn "irc" "pidgin"
               spawnOn "emacs" "mate-terminal"
               spawnOn "emacs" "atom"
             -- hack: ewmh props don't get set until something forces logHook, so...
             asks (logHook . config) >>= id
           }
           `additionalKeysP`
           ([("M-C-n",      namedScratchpadAction scratchpads "notes1")
            ,("M-C-t",      namedScratchpadAction scratchpads "timelog")
            ,("M-C-3",      namedScratchpadAction scratchpads "notes3")
            ,("M-C-4",      namedScratchpadAction scratchpads "notes4")
            ,("M-C-5",      namedScratchpadAction scratchpads "notes5")
             -- yes, I considered Prompt.Shell. usually want a terminal though...
            ,("M-x",        namedScratchpadAction scratchpads "qterm")
            ,("M-C-k",      namedScratchpadAction scratchpads "calc")
            ,("M-C-m",      namedScratchpadAction scratchpads "charmap")
            ,("C-`",        namedScratchpadAction scratchpads "mtr")
            ,("M-C-c",      namedScratchpadAction scratchpads "crawl")
            ,("<XF86Sleep>",unGrab >> spawn "xscreensaver-command -lock")
            ,("M-C-g",      spawn spawnChrome)
            ,("M-<Right>",  moveTo Next HiddenWS)
            ,("M-<Left>",   moveTo Prev HiddenWS)
            ,("M-S-`",      withFocused $ sendMessage . maximizeRestore)
            ,("M-S-p",      mateRun)
            ,("M-p",        shellPrompt greenXPConfig {promptKeymap = emacsLikeXPKeymap})
             -- multiple-screen shot
            ,("M-S-s",      unGrab >> spawn "scrot -m 'Downloads/screenshotM-%Y%m%dT%H%M%S.png'")
             -- focused window shot
            ,("M-S-w",      unGrab >> spawn "scrot -u 'Downloads/screenshotF-%Y%m%dT%H%M%S.png'")
             -- debug windows; also see M-S-d above
            ,("M-C-S-8",    withFocused showWinRR)
            ,("M-C-S-7",    spawn "xprop | xmessage -file -")
            ,("M-C-S-6",    withFocused $ \w -> spawn $ "xprop -id 0x" ++ showHex w "" ++ " | xmessage -file -")
            ,("M-C-S-5",    withFocused $ \w -> spawn $ "xwininfo -id 0x" ++ showHex w "" ++ " -all | xmessage -file -")
--              -- @@@ because of HackDocks
--             ,("M-b",       sendMessage ToggleStruts)
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

spawnChrome   = "google-chrome --allow-file-access-from-files"

myPlaceHook = inBounds $ smart (0.5, 0.5)

doFloatPlace :: ManageHook
doFloatPlace = placeHook myPlaceHook <+> doFloat

pidgin :: Property
pidgin = Resource "Pidgin" `And` Role "buddy_list"

ims = renamed [CutWordsRight 2] $
      OneBig (4/5) (3/4) |||
      StackTile 1 (3/100) (1/2) |||
      Grid 0.2 |||
      Accordion |||
      Full

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

-- haaaaaack
noTaskBar :: ManageHook
noTaskBar = ask >>= (>> idHook) . liftX . markNoTaskBar

markNoTaskBar :: Window -> X ()
markNoTaskBar w = withDisplay $ \d -> do
                    ws <- getAtom "_NET_WM_STATE"
                    ntb <- getAtom "_NET_WM_STATE_SKIP_TASKBAR"
                    npg <- getAtom "_NET_WM_STATE_SKIP_PAGER"
                    wst' <- io $ getWindowProperty32 d ws w
                    -- @@@ possibly this could just be Prepend...
                    let wst = case wst' of
                                Nothing -> [fi ntb,fi npg]
                                Just s  -> fi ntb:fi npg:s
                    io $ changeProperty32 d w ws aTOM propModeReplace wst

setUtility :: ManageHook
setUtility = ask >>= (>> idHook) . liftX . markUtility

markUtility :: Window -> X ()
markUtility w = withDisplay $ \d -> do
                    wt <- getAtom "_NET_WM_WINDOW_TYPE"
                    wtu <- getAtom "_NET_WM_WINDOW_TYPE_UTILITY"
                    io $ changeProperty32 d w wt aTOM propModeReplace [fi wtu]

-- sigh
fi :: (Integral i, Num n) => i -> n
fi = fromIntegral

logTitle :: D.Client -> X ()
logTitle ch = dynamicLogWithPP defaultPP
                               {ppCurrent = ("«" ++) . (++ "»")
                               ,ppVisible = id
                               ,ppHidden  = const ""
                               ,ppHiddenNoWindows = const ""
                               ,ppUrgent  = ("¡" ++) . (++ "!")
                               ,ppWsSep   = " "
                               ,ppSep     = "⋮"
                               ,ppSort    = getSortByXineramaPhysicalRule
                               ,ppOutput  = dbusOutput ch
                               }

getWellKnownName :: D.Client -> IO ()
getWellKnownName ch = do
  D.requestName ch (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput ch s = do
  let sig = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update")
            {D.signalBody = [D.toVariant (UTF8.decodeString (unPango s))]}
  D.emit ch sig

-- quick and dirty escaping of HTMLish Pango markup
unPango :: String -> String
unPango ('<':xs) = "&lt;" ++ unPango xs
unPango ('&':xs) = "&amp;" ++ unPango xs
unPango ('>':xs) = "&gt;" ++ unPango xs
unPango (x  :xs) = x:unPango xs
unPango []       = []

-- boing :: String -> Query ()
-- boing snd = liftX $ spawn $ "paplay /usr/share/sounds/freedesktop/stereo/" ++ snd ++ ".oga"

debuggering :: Event -> X All
-- debuggering = debugEventsHook
debuggering = idHook

-- testing this

-- produce a RationalRect describing a window.
-- note that we don't use getWindowAttributes because it's broken...
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
                  then Nothing
                  else Just $ W.RationalRect ((wx - rx) % rw) ((wy - ry) % rh) (ww % rw) (wh % rh)
      return $ case catMaybes rs of
                (r:_) -> Just r
                _     -> Nothing

showWinRR :: Window -> X ()
showWinRR w = do
  p <- spawnPipe "xmessage"
  getWinRR w >>= io . hPutStrLn p . show
  io $ hClose p
