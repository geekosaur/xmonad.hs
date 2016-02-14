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
-- import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.HackDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.Place
import           XMonad.Layout.Accordion
-- import           XMonad.Layout.DragPane
import           XMonad.Layout.GridVariants
import           XMonad.Layout.IM
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.OneBig
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.StackTile
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPane
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
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
import           Graphics.X11.Xlib.Misc                   (ungrabKeyboard, ungrabPointer)

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
                  "leafpad --name=timelog ~/Documents/SNA/timelog.txt"
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
              ]

workspacen :: [String]
workspacen =  ["1", "mail", "chrome", "emacs", "5", "6", "7", "8", "im", "0", "misc", "spare"]

main = do
  -- something is undoing this. regularly.
  -- make shift-space = space
  spawn "xmodmap -e 'keycode 65 = space space space space NoSymbol NoSymbol thinspace nobreakspace'"
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
                                avoidStruts $
                                lessBorders OnlyFloat $
                                onWorkspace "im" (withIM 0.125 pidgin
                                                  (reflectHoriz $ withIM 0.2 sipphone ims)) $
                                onWorkspace "8" Full $
                                onWorkspace "log" Full $
                                TwoPane 0.03 0.5 |||
                                -- dragPane Horizontal 0.03 0.5 |||
                                simpleTabbed |||
                                Full
           ,manageHook        = composeAll
                                [appName =? "Pidgin" --> doShift "im"
                                ,appName =? "xmessage" --> doFloatPlace
                                ,appName =? "trashapplet" --> doFloatPlace
                                ,appName =? "callinfo" --> doRectFloat (W.RationalRect 0.68 0.03 0.3 0.75)
                                -- @@@ copyToAll, but need a window parameter...
                                ,isInProperty "_NET_WM_STATE" "_NET_WM_STATE_STICKY" --> doIgnore
                                -- ,appName =? "Pidgin" <&&> role =? "conversation" --> boing "phone-incoming-call"
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
               spawnOn "mail" spawnChrome
               spawnOn "irc" "pidgin"
               spawnOn "emacs" "mate-terminal"
               spawnOn "emacs" "emacs"
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
            ,("M-C-g",      spawn spawnChrome)
            ,("M-C-l",      spawn "$HOME/.bin/callinfo")
            ,("M-C-c",      spawn "$HOME/.bin/callinfo --next")
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
            -- ,("M-C-S-8",    withFocused showWinRR)
            ,("M-C-S-7",    spawn "xprop | xmessage -file -")
            ,("M-C-S-6",    withFocused $ \w -> spawn $ "xprop -id 0x" ++ showHex w "" ++ " | xmessage -file -")
            ,("M-C-S-5",    withFocused $ \w -> spawn $ "xwininfo -id 0x" ++ showHex w "" ++ " -all | xmessage -file -")
             -- @@@ because of HackDocks
            ,("M-b",       sendMessage ToggleStruts)
            ]
            ++
            -- greedyView -> view, so I stop breaking crawl etc. >.>
            [(otherModMasks ++ "M-" ++ [key], action tag)
             | (tag, key)              <- zip workspacen "1234567890-="
             , (otherModMasks, action) <- [("", windows . W.view) -- was W.greedyView
                                          ,("S-", windows . W.shift)
                                          ]
            ])

spawnChrome   = "google-chrome --allow-file-access-from-files"

myPlaceHook = inBounds $ smart (0.5, 0.5)

doFloatPlace :: ManageHook
doFloatPlace = placeHook myPlaceHook <+> doFloat

pidgin :: Property
pidgin = Resource "Pidgin" `And` Role "buddy_list"

sipphone :: Property
sipphone = Resource "sflphone-client-gnome" `And` Title "SFLphone VoIP Client" -- aargh

ims = renamed [CutWordsRight 2] $
      OneBig (4/5) (3/4) |||
      StackTile 1 (3/100) (1/2) |||
      TwoPane 0.03 0.5 |||
      Grid 0.2 |||
      Accordion |||
      Full

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

unGrab :: X ()
unGrab = withDisplay $ \d -> io (ungrabKeyboard d currentTime >> ungrabPointer d currentTime)

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
