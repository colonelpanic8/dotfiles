{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Graphics.X11.ExtraTypes.XF86
import System.Directory
import System.FilePath.Posix
import System.Taffybar.Hooks.PagerHints (pagerHints)
import Text.Printf

import XMonad hiding ( (|||) )
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DWO
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Actions.WorkspaceNames
import XMonad.Config ()
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Layout.BoringWindows
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Minimize
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.CustomKeys
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.NamedWindows (getName)

getClass :: Window -> X String
getClass w = do
  classHint <- withDisplay $ \d -> io $ getClassHint d w
  return $ resClass classHint

myDecorateName ws w = do
  name <- show <$> getName w
  classTitle <- getClass w
  workspaceToName <- getWorkspaceNames
  return $ printf "%-20s%-40s %+30s" classTitle (take 40 name) "in " ++ workspaceToName (W.tag ws)

myWindowBringerConfig = WindowBringerConfig { menuCommand = "rofi"
                                            , menuArgs = ["-dmenu", "-i"]
                                            , windowTitler = myDecorateName
                                            }

getClassRemap :: IO (M.Map String String)
getClassRemap = do
  home <- getHomeDirectory
  text <- B.readFile (home </> ".lib/class_remap.json")
  return $ fromMaybe M.empty (decode text)

main = xmonad $ ewmh $ pagerHints def
       { modMask = mod4Mask
       , terminal = "urxvt"
       , manageHook = manageDocks <+> manageHook def
       , layoutHook = myLayoutHook
       , logHook = myLogHook
       , handleEventHook = docksEventHook <+> fullscreenEventHook
       , startupHook = myStartup
       , keys = customKeys (const []) addKeys
       }

myLogHook = fadeInactiveLogHook 0.9

setWorkspaceNameToFocusedWindow workspace  = do
  namedWindows <- mapM getClass $ W.integrate' $ W.stack workspace
  renamedWindows <- remapNames namedWindows
  WorkspaceNames namesMap <- XS.get
  let newName = intercalate "|" renamedWindows
      currentName = M.findWithDefault "" (W.tag workspace) namesMap
  when (currentName /= newName) $ setWorkspaceName (W.tag workspace) newName

remapNames namedWindows = do
  remap <- io getClassRemap
  return $ map (\original -> M.findWithDefault original original remap) namedWindows

setWorkspaceNames = do
  ws <- gets windowset
  mapM_ setWorkspaceNameToFocusedWindow (W.workspaces ws)

data WorkspaceNamesHook a = WorkspaceNamesHook deriving (Show, Read)

instance LayoutModifier WorkspaceNamesHook Window where
    hook _ = setWorkspaceNames

workspaceNamesHook = ModifiedLayout WorkspaceNamesHook

shiftThenView i = W.greedyView i . W.shift i

layouts = multiCol [1, 1] 2 0.01 (-0.5) ||| Full ||| Tall 1 (3/100) (1/2)

myLayoutHook = avoidStruts . smartSpacing 10 . noBorders . minimize .
               boringWindows . mkToggle (MIRROR ?? EOT) . workspaceNamesHook
                                 $ layouts

myStartup = spawn "systemctl --user start wm.target"

-- Use greedyView to switch to the correct workspace, and then focus on the
-- appropriate window within that workspace.
greedyFocusWindow w ws = W.focusWindow w $ W.greedyView
                         (fromMaybe (W.currentTag ws) $ W.findTag w ws) ws

shiftToEmptyAndView = doTo Next EmptyWS DWO.getSortByOrder
                      (windows . shiftThenView)

addKeys conf@XConfig {modMask = modm} =
    [ ((modm, xK_p), spawn "rofi -show drun")
    , ((modm .|. shiftMask, xK_p), spawn "rofi -show run")
    , ((modm, xK_g), actionMenu myWindowBringerConfig greedyFocusWindow)
    , ((modm, xK_b), bringMenuConfig myWindowBringerConfig)
    , ((modm .|. controlMask, xK_t), spawn
       "systemctl --user restart taffybar.service")
    , ((modm, xK_v), spawn "copyq paste")
    , ((modm, xK_s), swapNextScreen)
    , ((modm .|. controlMask, xK_space), sendMessage $ JumpToLayout "Full")
    , ((modm, xK_slash), sendMessage $ Toggle MIRROR)
    , ((modm, xK_m), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
    , ((modm, xK_backslash), toggleWS)

    -- App shortcuts
    , ((modalt, xK_s), raiseNextMaybe (spawn "spotify") (className =? "Spotify"))
    , ((modalt, xK_e), raiseNextMaybe (spawn "emacsclient -c") (className =? "Emacs"))
    , ((modalt, xK_h), raiseNextMaybe (spawn "google-chrome") (className =? "google-chrome"))
    , ((modalt, xK_h), raiseNextMaybe (spawn "cool")
                         (className =? "google-chrome"))

    -- Hyper bindings
    , ((mod3Mask, xK_1), setWorkspaceNames)
    , ((mod3Mask, xK_e), moveTo Next EmptyWS )
    , ((mod3Mask .|. shiftMask, xK_e), shiftToEmptyAndView)
    , ((mod3Mask, xK_v), spawn "copyq_rofi.sh")
    , ((mod3Mask, xK_p), spawn "system_password.sh")
    , ((mod3Mask, xK_h), spawn "screenshot.sh")
    , ((mod3Mask, xK_c), spawn "shell_command.sh")

    -- playerctl
    , ((mod3Mask, xK_f), spawn "playerctl play-pause")
    , ((0, xF86XK_AudioPause), spawn "playerctl play-pause")
    , ((mod3Mask, xK_d), spawn "playerctl next")
    , ((0, xF86XK_AudioNext), spawn "playerctl next")
    , ((mod3Mask, xK_a), spawn "playerctl previous")
    , ((0, xF86XK_AudioPrev), spawn "playerctl previous")

    -- volume control
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +05%")
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -05%")
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")
    , ((mod3Mask, xK_w), spawn "pactl set-sink-volume 0 +05%")
    , ((mod3Mask, xK_s), spawn "pactl set-sink-volume 0 -05%")

    ] ++

    -- Replace original moving stuff around + greedy view bindings
    [((additionalMask .|. modm, key), windows $ function workspace)
         | (workspace, key) <- zip (workspaces conf) [xK_1 .. xK_9]
         , (function, additionalMask) <-
             [ (W.greedyView, 0)
             , (W.shift, shiftMask)
             , (shiftThenView, controlMask)]]
    where
      modalt = modm .|. mod1Mask

-- Local Variables:
-- flycheck-ghc-args: ("-Wno-missing-signatures")
-- End:
