import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.Minimize
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

startup :: X ()
startup = do
    setWMName "LG3D"
    spawn "xrdb .Xresources"
    spawn "xsetroot -solid black"
    spawn "urxvtd --quiet --opendisplay --fork"

main = xmonad $ gnomeConfig {
  layoutHook = smartBorders $ layoutHook gnomeConfig,
  terminal = "urxvtc",
  borderWidth = 4,
  focusFollowsMouse = False,
  manageHook = composeOne
              [ className =? "grDebug" -?> doFloat
              , className =? "grRelease" -?> doFloat
              , isFullscreen -?> doFullFloat ],
  startupHook = startup
  }
  `removeKeysP`
       [ "M-k", "M-n", "M-m", "M-w", "M-q", "M-e", "M-r",
         "M-S-/", "M-S-w", "M-S-w", "M-S-e", "M-S-j", "M-S-k",
         "M-q",     -- restart (xmonad --recompile && xmonad --restart)
         "M-p",     -- launch dmenu
         "M-S-p",   -- launch gmrun
         "M-b",     -- toggle the status bar gap
         "M-/",
         "M-,", "M-."   -- increment/decrement the number of windows in master area
         -- "M-h", "M-l",   -- resize (left/right)
         -- "M-t",     -- toggle tiling
       ]
