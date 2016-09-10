import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Config()
import XMonad.Util.CustomKeys

main :: IO ()
main = xmonad defaultConfig
        { modMask = mod4Mask -- Use Super instead of Alt
        , keys = customKeys delkeys inskeys
        }
    where
      delkeys :: XConfig l -> [(KeyMask, KeySym)]
      delkeys _ = [ ]

      inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
      inskeys conf =
          [ ((mod4Mask, xK_g), gotoMenu)
          , ((mod4Mask, xK_b), bringMenu)
          ]
