import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import System.IO

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
myLayoutHook = avoidStruts ( simpleTabbed ||| tiled ||| Mirror tiled ) ||| noBorders (fullscreenFull Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 2/3
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

-- Main configuration, override the defaults to your liking.
myConfig = desktopConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = myLayoutHook
        , handleEventHook = XMonad.Layout.Fullscreen.fullscreenEventHook
        , workspaces = myWorkspaces
        , terminal           = myTerminal
        , borderWidth        = 2
        , normalBorderColor  = "#cccccc"
        , focusedBorderColor = "#cd8b00"
        , modMask = myMod     -- Rebind Mod to the Windows key
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

myKeysP =   [ ("<XF86Calculator>", spawn "xlock -mode matrix")
            , ("<XF86Explorer>", spawn "thunar")
            , ("<XF86Tools>", spawn "setxkbmap gb")
            , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%-")
            , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+")
            , ("<XF86AudioMute>", spawn "amixer -q sset Master toggle")
            , ("<XF86AudioPlay>", spawn "mpc toggle")
            , ("<XF86AudioPrev>", spawn "mpc prev")
            , ("<XF86AudioNext>", spawn "mpc next")
            , ("M-p", spawn "dmenu_run -b") ]
            ++
            [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
                 | (key, scr)  <- zip "wer" [1,0,2] -- was [0..] *** change to match your screen order ***
                 , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
            ]

-- vim: ft=haskell
