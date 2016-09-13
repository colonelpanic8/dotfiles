import System.IO

import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Config()
import XMonad.Hooks.DynamicLog
import XMonad.Util.CustomKeys
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import qualified Data.Map        as M

startup :: X()
startup = do
  spawn "stalonetray"
  spawn "xscreensaver -no-splash"
  spawn "feh --bg-scale /usr/share/backgrounds/gnome/Blinds.jpg"
  spawn "copyq"



main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ ewmh defaultConfig
       { modMask = mod4Mask -- Use Super instead of Alt
        , keys = customKeys delkeys inskeys
        , terminal = "urxvt"
        , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "green" "" . shorten 50
                    }
        , handleEventHook =
            handleEventHook defaultConfig <+> fullscreenEventHook
       , startupHook = startup
        }
    where
      delkeys :: XConfig l -> [(KeyMask, KeySym)]
      delkeys _ = [ ]

      inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
      inskeys conf@XConfig {XMonad.modMask = modm} =
          [ ((modm, xK_p), spawn "rofi -show drun")
          , ((modm, xK_g), spawn "rofi -show window")
          , ((modm, xK_b), spawn "rofi -show run")
          ]
