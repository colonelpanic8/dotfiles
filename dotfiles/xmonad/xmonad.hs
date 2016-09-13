import System.IO

import XMonad
import XMonad.Config()
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)

-- Use Super/Command/WinKey instead of Alt
myModMask :: KeyMask
myModMask = mod4Mask

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ ewmh def
       { modMask = myModMask
       , terminal = "urxvt"
       , layoutHook = avoidStruts $ layoutHook def
       , logHook = dynamicLogWithPP xmobarPP
         { ppOutput = hPutStrLn xmproc
         , ppTitle = xmobarColor "green" "" . shorten 50
         }
       , handleEventHook = handleEventHook def <+> fullscreenEventHook
       , startupHook = myStartup
        } `additionalKeys`
       [ ((myModMask, xK_p), spawn "rofi -show drun")
       , ((myModMask, xK_g), spawn "rofi -show window")
         -- TODO: Change this to bringing the window to the current workspace
       , ((myModMask, xK_b), spawn "rofi -show run")
       ]

myStartup :: X()
myStartup = do
  spawn "stalonetray"
  spawn "xsetroot -solid black"
  -- TODO: Figure out how to set different backgrounds for different x
  -- monitors
  -- spawn "feh --bg-scale /usr/share/backgrounds/gnome/Blinds.jpg"
  spawn "copyq"
