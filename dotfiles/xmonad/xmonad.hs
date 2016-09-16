import XMonad hiding ( (|||) )
import XMonad.Config ()
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Util.CustomKeys

main = xmonad $ ewmh def
       { modMask = mod4Mask
       , terminal = "urxvt"
       , manageHook = manageDocks <+> manageHook def
       , layoutHook = myLayoutHook
       -- , logHook = myLogHook topBar
       , handleEventHook = handleEventHook def <+> fullscreenEventHook
       , startupHook = myStartup
       , keys = customKeys delKeys addKeys
       }

delKeys _ = []

addKeys XConfig {modMask = modm} =
    [ ((modm, xK_p), spawn "rofi -show drun")
    , ((modm, xK_g), spawn "rofi -show window")
    -- , ((modm, xK_s), sequence_ [shiftNextScreen, nextScreen])
    -- TODO: Change this to bringing the window to the current workspace
    , ((modm, xK_b), spawn "rofi -show run")
    , ((modm .|. controlMask, xK_space), sendMessage $ JumpToLayout "Full")
    , ((modm, xK_slash), sendMessage $ Toggle MIRROR)
    ]

layouts = tiled ||| Full ||| ThreeCol 1 (3/100) (1/3)
          where
            tiled = Tall 1 (3/100) (1/2)

myLayoutHook = avoidStruts . smartSpacing 10 . noBorders
               . mkToggle (MIRROR ?? EOT) $ layouts

myStartup = do
  spawn "nm-applet --sm-disable"
  spawn "xsetroot -solid black"
  -- TODO: Figure out how to set different backgrounds for different x
  -- monitors
  -- spawn "feh --bg-scale /usr/share/backgrounds/gnome/Blinds.jpg"
  spawn "copyq"
