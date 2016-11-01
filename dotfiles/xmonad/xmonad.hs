{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module Main where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Graphics.X11.ExtraTypes.XF86
import System.Directory
import System.FilePath.Posix
import System.Taffybar.Hooks.PagerHints
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


main = xmonad $ def
       { modMask = mod4Mask
       , terminal = "urxvt"
       , manageHook = manageDocks <+> myManageHook <+> manageHook def
       , layoutHook = myLayoutHook
       , logHook = toggleFadeInactiveLogHook 0.9 +++ ewmhWorkspaceNamesLogHook
       , handleEventHook = docksEventHook <+> fullscreenEventHook +++
                           ewmhDesktopsEventHook +++ pagerHintsEventHook
       , startupHook = myStartup +++ ewmhWorkspaceNamesLogHook
       , keys = customKeys (const []) addKeys
      } where
    x +++ y = mappend y x

isHangoutsTitle = isPrefixOf "Google Hangouts"

chromeSelectorBase = className =? "Google-chrome"
chromeSelector = chromeSelectorBase <&&>
                 fmap (not . isHangoutsTitle) title
spotifySelector = className =? "Spotify"
emacsSelector = className =? "Emacs"
transmissionSelector = fmap (isPrefixOf "Transmission") title
hangoutsSelector = chromeSelectorBase <&&>
                   fmap isHangoutsTitle title

virtualClasses = [ (hangoutsSelector, "Hangouts")
                 , (chromeSelector, "Chrome")
                 , (transmissionSelector, "Transmission")
                 ]

-- Startup

myStartup = spawn "systemctl --user start wm.target"

-- Manage

myManageHook = composeAll . concat $
               [ [ hangoutsSelector --> doShift "3"] ]

-- Layout

layouts = multiCol [1, 1] 2 0.01 (-0.5) ||| Full |||
          Tall 1 (3/100) (1/2) ||| Tall 1 (3/100) (3/4)

myLayoutHook = avoidStruts . smartSpacing 10 . minimize . boringWindows .
               mkToggle (MIRROR ?? EOT) . workspaceNamesHook . smartBorders .
               noBorders $ layouts

-- WindowBringer

myWindowBringerConfig = WindowBringerConfig { menuCommand = "rofi"
                                            , menuArgs = ["-dmenu", "-i"]
                                            , windowTitler = myDecorateName
                                            }

findM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM f = runMaybeT . msum . map (MaybeT . f)

classIfMatches window entry = do
  result <- runQuery (fst entry) window
  return $ if result then Just $ snd entry else Nothing

getClass w = do
  virtualClass <- findM (classIfMatches w) virtualClasses
  case virtualClass of
    Nothing -> do
               classHint <- withDisplay $ \d -> io $ getClassHint d w
               return $ resClass classHint
    Just name -> return name

myDecorateName ws w = do
  name <- show <$> getName w
  classTitle <- getClass w
  workspaceToName <- getWorkspaceNames
  return $ printf "%-20s%-40s %+30s" classTitle (take 40 name)
             "in " ++ workspaceToName (W.tag ws)

-- Dynamic Workspace Renaming

getClassRemap = do
  home <- getHomeDirectory
  -- TODO: handle the case where this file does not exist
  text <- B.readFile $ home </> ".lib/resources/window_class_to_fontawesome.json"
  return $ fromMaybe M.empty (decode text)

setWorkspaceNameToFocusedWindow workspace  = do
  namedWindows <- mapM getClass $ W.integrate' $ W.stack workspace
  renamedWindows <- remapNames namedWindows
  WorkspaceNames namesMap <- XS.get
  let newName = intercalate "|" renamedWindows
      currentName = M.findWithDefault "" (W.tag workspace) namesMap
  when (currentName /= newName) $ setWorkspaceName (W.tag workspace) newName

remapNames namedWindows = do
  remap <- io getClassRemap
  return $ map (\orig -> M.findWithDefault orig orig remap) namedWindows

setWorkspaceNames = do
  ws <- gets windowset
  mapM_ setWorkspaceNameToFocusedWindow (W.workspaces ws)

data WorkspaceNamesHook a = WorkspaceNamesHook deriving (Show, Read)

instance LayoutModifier WorkspaceNamesHook Window where
    hook _ = setWorkspaceNames

workspaceNamesHook = ModifiedLayout WorkspaceNamesHook

-- EWMH support for workspace names

ewmhWorkspaceNamesLogHook = do
  WorkspaceNames namesMap <- XS.get
  let tagRemapping = getWorkspaceNameFromTag namesMap
  pagerHintsLogHookCustom tagRemapping
  ewmhDesktopsLogHookCustom id tagRemapping

getWorkspaceNameFromTag namesMap tag =
    case M.lookup tag namesMap of
      Nothing -> tag
      Just label -> printf "%s: %s " tag label

shiftThenView i = W.greedyView i . W.shift i

-- Toggleable fade

newtype ToggleFade = ToggleFade (M.Map Window Bool)
    deriving (Typeable, Read, Show)

instance ExtensionClass ToggleFade where
    initialValue = ToggleFade M.empty
    extensionType = PersistentExtension

fadeEnabledForWindow = ask >>= \w -> liftX (do
                         ToggleFade toggleMap <- XS.get
                         return $ M.findWithDefault True w toggleMap)

toggleFadeInactiveLogHook = fadeOutLogHook . fadeIf (isUnfocused <&&> fadeEnabledForWindow)

toggleFadingForActiveWindow = withWindowSet $ \windowSet -> do
  ToggleFade toggleMap <- XS.get
  let maybeWindow = W.peek windowSet
  case maybeWindow of
    Nothing -> return ()
    Just window -> do
        let existingValue = M.findWithDefault True window toggleMap
        XS.put $ ToggleFade (M.insert window (not existingValue) toggleMap)
        return ()

-- Use greedyView to switch to the correct workspace, and then focus on the
-- appropriate window within that workspace.
greedyFocusWindow w ws = W.focusWindow w $ W.greedyView
                         (fromMaybe (W.currentTag ws) $ W.findTag w ws) ws

shiftToEmptyAndView = doTo Next EmptyWS DWO.getSortByOrder (windows . shiftThenView)

myRaiseNextMaybe = raiseNextMaybeCustomFocus greedyFocusWindow
myBringNextMaybe = raiseNextMaybeCustomFocus bringWindow

bindBringAndRaise :: KeyMask -> KeySym -> X () -> Query Bool -> [((KeyMask, KeySym), X ())]
bindBringAndRaise mask sym start query =
    [ ((mask, sym), myRaiseNextMaybe start query)
    , ((mask .|. controlMask, sym), myBringNextMaybe start query)]

bindBringAndRaiseMany :: [(KeyMask, KeySym, X (), Query Bool)] -> [((KeyMask, KeySym), X())]
bindBringAndRaiseMany = concatMap (\(a, b, c, d) -> bindBringAndRaise a b c d)

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

    -- Hyper bindings
    , ((mod3Mask, xK_1), toggleFadingForActiveWindow)
    , ((mod3Mask, xK_e), moveTo Next EmptyWS )
    , ((mod3Mask .|. shiftMask, xK_e), shiftToEmptyAndView)
    , ((mod3Mask, xK_v), spawn "copyq_rofi.sh")
    , ((mod3Mask, xK_p), spawn "system_password.sh")
    , ((mod3Mask, xK_h), spawn "screenshot.sh")
    , ((mod3Mask, xK_c), spawn "shell_command.sh")

    -- ModAlt bindings
    , ((modalt, xK_w), spawn "rofi_wallpaper.sh")

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

    ] ++ bindBringAndRaiseMany

    [ (modalt, xK_s, spawn "spotify", spotifySelector)
    , (modalt, xK_e, spawn "emacsclient -c", emacsSelector)
    , (modalt, xK_c, spawn "google-chrome-stable", chromeSelector)
    , (modalt, xK_h, spawn "cool", hangoutsSelector)
    , (modalt, xK_t, spawn "transmission", transmissionSelector)
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
