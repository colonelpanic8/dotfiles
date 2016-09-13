import System.IO

import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Config()
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.CustomKeys
import XMonad.Util.Run(spawnPipe)

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad defaultConfig
       { modMask = mod4Mask -- Use Super instead of Alt
        , keys = customKeys delkeys inskeys
        , terminal = "urxvt"
        , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "green" "" . shorten 50
                    }
        }
    where
      delkeys :: XConfig l -> [(KeyMask, KeySym)]
      delkeys _ = [ ]

      inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
      inskeys conf =
          [ ((mod4Mask, xK_g), gotoMenu)
          , ((modM4ask, xK_b), bringMenu)
          ]
