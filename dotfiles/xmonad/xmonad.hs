import XMonad
import XMonad.Config()
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig


-- Use Super/Command/WinKey instead of Alt
myModMask :: KeyMask
myModMask = mod4Mask

main :: IO ()
main = xmonad $ ewmh def
       { modMask = myModMask
       , terminal = "urxvt"
       , manageHook = manageDocks <+> manageHook def
       , layoutHook = myLayoutHook
       -- , logHook = myLogHook topBar
       , handleEventHook = handleEventHook def <+> fullscreenEventHook
       , startupHook = myStartup
        } `additionalKeys`
       [ ((myModMask, xK_p), spawn "rofi -show drun")
       , ((myModMask, xK_g), spawn "rofi -show window")
         -- TODO: Change this to bringing the window to the current workspace
       , ((myModMask, xK_b), spawn "rofi -show run")
       , ((myModMask .|. controlMask, xK_space), sendMessage $ Toggle FULL)
       ]

myLayoutHook = avoidStruts . smartSpacing 10 . noBorders
               . mkToggle (FULL ?? EOT) $
               Tall 1 (3/100) (1/2) ||| ThreeCol 1 (3/100) (1/3)


myStartup :: X()
myStartup = do
  spawn "nm-applet --sm-disable"
  spawn "xsetroot -solid black"
  -- TODO: Figure out how to set different backgrounds for different x
  -- monitors
  -- spawn "feh --bg-scale /usr/share/backgrounds/gnome/Blinds.jpg"
  spawn "copyq"
