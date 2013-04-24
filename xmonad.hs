import XMonad
import qualified XMonad.StackSet as W
import Data.Ratio ((%))
import System.IO
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Layout.Grid
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.Window
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)

myWorkspaces = map show [1..22]
myMod        = mod4Mask
myTerminal   = "Terminal"

-- one line down    xmproc <- spawnPipe "/usr/bin/xmobar /home/greg/.xmobarrc"
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Layouthook
myLayoutHook = avoidStruts $ smartBorders ( tiled
                           ||| Mirror tiled
                           ||| simpleTabbed
                           ||| Grid
                           ||| fullscreenFull Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 2/3
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

-- Manage hook
myManageHook :: ManageHook
myManageHook = manageDocks <+> (composeAll . concat $
    [ [resource     =? r     --> doIgnore                      |   r   <- myIgnores]
--    , [className    =? c     --> doShift (myWorkspaces !! 1)   |   c   <- myWebS   ]
--    , [className    =? c     --> doShift (myWorkspaces !! 4)   |   c   <- myChatS  ]
--    , [className    =? c     --> doShift (myWorkspaces !! 3)   |   c   <- myGfxS   ]
--    , [className    =? c     --> doShift (myWorkspaces !! 5)   |   c   <- myMusicS ]
--    , [className    =? c     --> doFloat                       |   c   <- myFloatFC]
--    , [className    =? c     --> doCenterFloat                 |   c   <- myFloatCC]
    , [name         =? n     --> doSideFloat NW                |   n   <- myFloatSN]
    , [name         =? n     --> doF W.focusDown               |   n   <- myFocusDC]
    , [role =? "pop-up" --> doSideFloat CE ]
    , [composeOne   [ isFullscreen -?> doFullFloat ]]
    ])
    where
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
        myIgnores = ["desktop","desktop_window"]
--        myWebS    = ["Chromium","Firefox"]
--        myGfxS    = ["gimp-2.6", "Gimp-2.6", "Gimp", "gimp", "GIMP"]
--        myChatS   = ["Pidgin", "Xchat"]
--        myMusicS  = ["Clementine"]
--        myFloatFC = ["Steam"]
--        myFloatCC = ["File-roller", "zsnes", "Gcalctool"]
        myFloatSN = ["Event Tester"]
        myFocusDC = ["xfce4-notifyd"]

-- Main configuration, override the defaults to your liking.
myConfig = desktopConfig
        { manageHook = myManageHook
        , layoutHook = myLayoutHook
        , handleEventHook = XMonad.Layout.Fullscreen.fullscreenEventHook
        , workspaces = myWorkspaces
        , terminal           = myTerminal
        , borderWidth        = 2
        , normalBorderColor  = "#cccccc"
        , focusedBorderColor = "#cd8b00"
        , modMask = myMod     -- Rebind Mod to the Windows key
        , startupHook = setWMName "LG3D" -- StartupHook for RubyMine
        , logHook = takeTopFocus -- LogHook for RubyMine
        }
        `additionalKeys` [
            ((myMod .|. controlMask, xK_Return), spawn myTerminal),
            ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s"),
            ((0, xK_Print), spawn "scrot"),
            ((myMod, xK_0),   windows $ W.greedyView "10"),
            ((myMod, xK_F1),  windows $ W.greedyView "11"),
            ((myMod, xK_F2),  windows $ W.greedyView "12"),
            ((myMod, xK_F3),  windows $ W.greedyView "13"),
            ((myMod, xK_F4),  windows $ W.greedyView "14"),
            ((myMod, xK_F5),  windows $ W.greedyView "15"),
            ((myMod, xK_F6),  windows $ W.greedyView "16"),
            ((myMod, xK_F7),  windows $ W.greedyView "17"),
            ((myMod, xK_F8),  windows $ W.greedyView "18"),
            ((myMod, xK_F9),  windows $ W.greedyView "19"),
            ((myMod, xK_F10), windows $ W.greedyView "20"),
            ((myMod, xK_F11), windows $ W.greedyView "21"),
            ((myMod, xK_F12), windows $ W.greedyView "22"),
            ((shiftMask .|. myMod, xK_0),   windows $ W.shift "10"),
            ((shiftMask .|. myMod, xK_F1),  windows $ W.shift "11"),
            ((shiftMask .|. myMod, xK_F2),  windows $ W.shift "12"),
            ((shiftMask .|. myMod, xK_F3),  windows $ W.shift "13"),
            ((shiftMask .|. myMod, xK_F4),  windows $ W.shift "14"),
            ((shiftMask .|. myMod, xK_F5),  windows $ W.shift "15"),
            ((shiftMask .|. myMod, xK_F6),  windows $ W.shift "16"),
            ((shiftMask .|. myMod, xK_F7),  windows $ W.shift "17"),
            ((shiftMask .|. myMod, xK_F8),  windows $ W.shift "18"),
            ((shiftMask .|. myMod, xK_F9),  windows $ W.shift "19"),
            ((shiftMask .|. myMod, xK_F10), windows $ W.shift "20"),
            ((shiftMask .|. myMod, xK_F11), windows $ W.shift "21"),
            ((shiftMask .|. myMod, xK_F12), windows $ W.shift "22")
        ]
        `additionalKeysP` myKeysP

myKeysP =   [ ("C-M-n", AL.launchApp defaultXPConfig "/home/greg/bin/trello-wrapper.sh " )
            , ("S-M-g", windowPromptGoto defaultXPConfig { autoComplete = Just 500000 } )
            , ("S-M-b", windowPromptBring defaultXPConfig )
            , ("M-p", spawn "dmenu_run -b")
            , ("C-M-t", spawn "transset-df -p")        -- transparency for the window under the cursor
            -- Work Keyboard
            , ("<XF86Calculator>", spawn "xlock -mode matrix") -- Work keyboard
            , ("<XF86Explorer>", spawn "thunar")
            , ("<XF86Tools>", spawn "setxkbmap gb")
            -- Laptop
            , ("M-<KP_Enter>", spawn "xscreensaver-command -lock") -- Laptop
            , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%-")
            , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+")
            , ("<XF86AudioMute>", spawn "amixer -q sset Master toggle")
            , ("<XF86AudioPlay>", spawn "mpc toggle")
            , ("<XF86AudioPrev>", spawn "mpc prev")
            , ("<XF86AudioNext>", spawn "mpc next")
            ]
            ++
            -- Workspaces
            [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
                 | (key, scr)  <- zip "wer" [1,0,2] -- was [0..] *** change to match your screen order ***
                 , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
            ]

-- vim: ft=haskell
