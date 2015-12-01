import XMonad
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
                
main = xmonad $ defaultConfig {
    modMask = mod4Mask,               -- use Super instead of Alt
    layoutHook = smartBorders $ layoutHook defaultConfig,
    terminal = "urxvtc",
    borderWidth = 4,
    focusFollowsMouse = False,
    manageHook = composeOne [ isFullscreen -?> doFullFloat ],
    startupHook = setWMName "LG3D"
    }
         
    `removeKeysP`
        [ "M-m", "M-w", "M-q", "M-e", "M-r",
          -- "M-h", "M-l",   -- resize (left/right)
          "M-S-w", "M-S-w", "M-S-e", 
          "M-q",     -- restart (xmonad --recompile && xmonad --restart)
          "M-p",     -- launch dmenu
          "M-S-p",   -- launch gmrun
          "M-b",     -- toggle the status bar gap
     --     "M-t",     -- toggle tiling
          "M-/",
          "M-,", "M-."   -- increment/decrement the number of windows in master area
        ]
