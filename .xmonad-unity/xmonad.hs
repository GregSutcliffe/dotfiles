import XMonad
import XMonad.Config.Gnome
import XMonad.ManageHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig

-- Variables
myMod = mod4Mask

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Do"             --> doFloat
    ])

main = xmonad $ gnomeConfig {
    manageHook         = myManageHook,
    handleEventHook    = fullscreenEventHook,
    modMask            = myMod,
    normalBorderColor  = "#FF0000",
    focusedBorderColor = "#46443F"
    }
    `additionalKeysP`
    [("M-p", spawn "gnome-do")]

 
