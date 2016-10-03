import System.Taffybar.Hooks.PagerHints (pagerHints)

import XMonad hiding ( (|||) )
import XMonad.Actions.WindowBringer
import XMonad.Config ()
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Layout.BoringWindows
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Minimize
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.CustomKeys

main = xmonad $ ewmh $ pagerHints def
       { modMask = mod4Mask
       , terminal = "urxvt"
       , manageHook = manageDocks <+> manageHook def
       , layoutHook = myLayoutHook
       , logHook = myLogHook
       , handleEventHook = docksEventHook <+> fullscreenEventHook
       , startupHook = myStartup
       , keys = customKeys (\x -> []) addKeys
       }

myLogHook :: X()
myLogHook = fadeInactiveLogHook 0.9

shiftThenView i = W.greedyView i . W.shift i

addKeys conf@XConfig {modMask = modm} =
    [ ((modm, xK_p), spawn "rofi -show drun")
    , ((modm .|. shiftMask, xK_p), spawn "rofi -show run")
    , ((modm, xK_g), spawn "rofi -show window")
    , ((modm .|. controlMask, xK_t), spawn "restart.sh taffybar")
    -- TODO: Change this to bringing the window to the current workspace
    , ((modm, xK_b), bringMenuArgs' "rofi" ["-dmenu"])
    , ((mod3Mask, xK_v), spawn "copyq_rofi.sh")
    , ((modm .|. controlMask, xK_space), sendMessage $ JumpToLayout "Full")
    , ((modm, xK_slash), sendMessage $ Toggle MIRROR)
    , ((modm, xK_m), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
    ] ++
    -- Replace original moving stuff around + greedy view bindings
    [((additionalMask .|. modm, key), windows $ function workspace)
         | (workspace, key) <- zip (workspaces conf) [xK_1 .. xK_9]
         , (function, additionalMask) <-
             [ (W.greedyView, 0)
             , (W.shift, shiftMask)
             , (shiftThenView, controlMask)]]

layouts = tiled ||| Full ||| multiCol [1, 1] 2 0.01 (-0.5)
          where
            tiled = Tall 1 (3/100) (1/2)

myLayoutHook = avoidStruts . smartSpacing 10 . noBorders . minimize
               . boringWindows . mkToggle (MIRROR ?? EOT) $ layouts

myStartup = startupHook def
