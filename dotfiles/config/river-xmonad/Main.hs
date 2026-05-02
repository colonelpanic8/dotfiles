{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Concurrent (forkIO)
import Data.Bits ((.&.), complement)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (find, foldl', isInfixOf, isPrefixOf, minimumBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Graphics.X11.ExtraTypes.XF86
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import System.Process (readCreateProcessWithExitCode, shell, spawnCommand, waitForProcess)
import XMonad
import qualified XMonad.Layout.Renamed as RN
import XMonad.River.WindowManager
import XMonad.River.WindowManager.Wayland
import qualified XMonad.StackSet as W

data Direction = DirectionUp | DirectionDown | DirectionLeft | DirectionRight
  deriving (Eq, Show)

data EqualColumns a = EqualColumns
  deriving (Read, Show, Typeable)

instance LayoutClass EqualColumns a where
  description _ = "Columns"
  pureLayout _ rect stack =
    zip windows (equalColumnRects rect (length windows))
    where
      windows = W.integrate stack

main :: IO ()
main = do
  let bindings = keyBindings
  configLog $ "starting imalison-river-xmonad with keybindings=" ++ show (length bindings)
  initialState <- initialRiverWMState riverConfig
  runRiverWMWaylandConfig
    RiverWMWaylandConfig
      { riverWMWaylandInitialState = initialState
      , riverWMWaylandKeyBindings = bindings
      }

riverLayouts =
  renamed "Columns" EqualColumns
  ||| Full
  where
    renamed name = RN.renamed [RN.Replace name]

riverConfig =
  (defaultRiverWMConfig riverLayouts)
    { riverWMWorkspaces = ordinaryWorkspaces ++ specialWorkspaces
    , riverWMMouseFollowsFocus = True
    , riverWMBorderWidth = 2
    , riverWMFocusedBorderColor = rgba8 0xed 0xb4 0x43 0xee
    , riverWMUnfocusedBorderColor = rgba8 0x59 0x59 0x59 0xaa
    }

rgba8 :: Word32 -> Word32 -> Word32 -> Word32 -> RiverWMColor
rgba8 red green blue alpha =
  RiverWMColor (wide red) (wide green) (wide blue) (wide alpha)
  where
    wide component = component * 0x01010101

keyBindings
  :: (LayoutClass l Window, Read (l Window))
  => [RiverWMWaylandKeyBinding l]
keyBindings =
  addHyperChordBindings hyper hyperChord $
    concat
      [ directionalBindings super directionalFocus
      , directionalBindings (super .|. shift) directionalSwap
      , directionalBindings (super .|. ctrl) (shiftFocusedToDirectionalScreen False)
      , directionalBindings (super .|. ctrl .|. shift) shiftFocusedToEmptyWorkspaceOnDirectionalScreen
      , directionalBindings hyper focusDirectionalScreen
      , directionalBindings (hyper .|. shift) (shiftFocusedToDirectionalScreen True)
      , workspaceBindings
      , layoutBindings
      , spawnBindings
      , mediaBindings
      ]

directionalBindings
  :: RiverWMWaylandModifiers
  -> (Direction -> RiverWMWaylandAction l)
  -> [RiverWMWaylandKeyBinding l]
directionalBindings mods command =
  [ key mods xK_w (command DirectionUp)
  , key mods xK_s (command DirectionDown)
  , key mods xK_a (command DirectionLeft)
  , key mods xK_d (command DirectionRight)
  ]

workspaceBindings
  :: [RiverWMWaylandKeyBinding l]
workspaceBindings =
  [ key (mods .|. super) keysym (action $ command workspace)
  | (workspace, keysym) <- zip (map show [(1 :: Int) .. 9]) [xK_1 .. xK_9]
  , (command, mods, action) <-
      [ (W.greedyView, noMods, stackAction)
      , (W.shift, shift, stackAction)
      , (\workspaceId stackSet -> W.greedyView workspaceId (W.shift workspaceId stackSet), ctrl, stackActionWarpPointer)
      ]
  ]

layoutBindings
  :: (LayoutClass l Window, Read (l Window))
  => [RiverWMWaylandKeyBinding l]
layoutBindings =
  [ key super xK_space (layoutAction NextLayout)
  , key (super .|. shift) xK_space (layoutAction (JumpToLayout "Columns"))
  , key (super .|. ctrl) xK_space (layoutAction (JumpToLayout "Full"))
  , key super xK_bracketleft (layoutAction Shrink)
  , key super xK_bracketright (layoutAction Expand)
  , key super xK_comma (layoutAction (IncMasterN 1))
  , key super xK_period (layoutAction (IncMasterN (-1)))
  ]

spawnBindings
  :: [RiverWMWaylandKeyBinding l]
spawnBindings =
  [ key super xK_Return (spawnAction "ghostty --gtk-single-instance=false")
  , key (super .|. shift) xK_Return (spawnAction "ghostty --gtk-single-instance=false")
  , key super xK_p (spawnAction "rofi -show drun -show-icons")
  , key (super .|. shift) xK_p (spawnAction "rofi -show run")
  , key super xK_Tab (selectWindowAction "windows" focusSelectedWindow)
  , key super xK_g (selectWindowAction "go to window" focusSelectedWindow)
  , key super xK_b (selectWindowAction "bring window" bringSelectedWindow)
  , key (super .|. shift) xK_b (selectWindowAction "replace window" replaceSelectedWindow)
  , key super xK_m minimizeFocusedWindow
  , key (super .|. shift) xK_m restoreLastMinimizedWindow
  , key super xK_q (spawnAction "river-xmonad-restart")
  , key (super .|. shift) xK_c closeFocusedWindow
  , key (super .|. shift) xK_q (spawnAction "riverctl exit")
  , key (super .|. alt) xK_e (toggleScratchpad "element")
  , key (super .|. alt) xK_h (toggleScratchpad "htop")
  , key (super .|. alt) xK_k (toggleScratchpad "slack")
  , key (super .|. alt) xK_s (toggleScratchpad "spotify")
  , key (super .|. alt) xK_t (toggleScratchpad "transmission")
  , key (super .|. alt) xK_v (toggleScratchpad "volume")
  , key (super .|. alt) xK_c (spawnAction "google-chrome-stable")
  , key super xK_e (spawnAction "emacsclient --eval '(emacs-everywhere)'")
  , key (super .|. ctrl) xK_e (shiftFocusedToNextEmptyWorkspace False)
  , key (super .|. shift) xK_e (shiftFocusedToNextEmptyWorkspace True)
  , key super xK_v (spawnAction "wl-paste | wtype -")
  , key hyper xK_e viewNextEmptyWorkspace
  , key hyper xK_v (spawnAction "rofi -modi 'clipboard:greenclip print' -show clipboard")
  , key hyper xK_p (spawnAction "rofi-pass")
  , key hyper xK_h (spawnAction "rofi_shutter")
  , key hyper xK_c (spawnAction "shell_command.sh")
  , key hyper xK_g gatherFocusedAppId
  , key hyper xK_x (spawnAction "rofi_command.sh")
  , key (hyper .|. shift) xK_l (spawnAction "loginctl lock-session")
  , key hyper xK_k (spawnAction "rofi_kill_process.sh")
  , key (hyper .|. shift) xK_k (spawnAction "rofi_kill_all.sh")
  , key hyper xK_r (spawnAction "rofi-systemd")
  , key hyper xK_9 (spawnAction "start_synergy.sh")
  , key hyper xK_backslash (spawnAction "$HOME/dotfiles/dotfiles/lib/functions/mpg341cx_input toggle")
  , key hyper xK_i (spawnAction "rofi_select_input.hs")
  , key hyper xK_o (spawnAction "rofi_paswitch")
  , key hyper xK_comma (spawnAction "rofi_wallpaper.sh")
  , key hyper xK_slash (spawnAction "toggle_taffybar")
  , key hyper xK_y (spawnAction "rofi_agentic_skill")
  ]

mediaBindings
  :: [RiverWMWaylandKeyBinding l]
mediaBindings =
  [ key super xK_semicolon (spawnAction "playerctl play-pause")
  , key noMods xF86XK_AudioPause (spawnAction "playerctl play-pause")
  , key noMods xF86XK_AudioPlay (spawnAction "playerctl play-pause")
  , key super xK_l (spawnAction "playerctl next")
  , key noMods xF86XK_AudioNext (spawnAction "playerctl next")
  , key super xK_j (spawnAction "playerctl previous")
  , key noMods xF86XK_AudioPrev (spawnAction "playerctl previous")
  , key noMods xF86XK_AudioRaiseVolume (spawnAction "set_volume --unmute --change-volume +5")
  , key noMods xF86XK_AudioLowerVolume (spawnAction "set_volume --unmute --change-volume -5")
  , key noMods xF86XK_AudioMute (spawnAction "set_volume --toggle-mute")
  , key super xK_i (spawnAction "set_volume --unmute --change-volume +5")
  , key super xK_k (spawnAction "set_volume --unmute --change-volume -5")
  , key super xK_u (spawnAction "set_volume --toggle-mute")
  , key (hyper .|. shift) xK_q (spawnAction "toggle_mute_current_window.sh")
  , key (hyper .|. ctrl) xK_q (spawnAction "toggle_mute_current_window.sh only")
  , key noMods xF86XK_MonBrightnessUp (spawnAction "brightness.sh up")
  , key noMods xF86XK_MonBrightnessDown (spawnAction "brightness.sh down")
  ]

key
  :: RiverWMWaylandModifiers
  -> KeySym
  -> RiverWMWaylandAction l
  -> RiverWMWaylandKeyBinding l
key modifiers keysym action =
  RiverWMWaylandKeyBinding
    { riverWMWaylandKeyModifiers = modifiers
    , riverWMWaylandKeyKeysym = fromIntegral keysym
    , riverWMWaylandKeyAction = action
    }

spawnAction :: String -> RiverWMWaylandAction l
spawnAction command state = do
  configLog $ "spawn start: " ++ command
  process <- spawnCommand (riverSpawnPrelude ++ command)
  _ <- forkIO $ do
    exitCode <- waitForProcess process
    configLog $ "spawn exit: " ++ command ++ " -> " ++ show exitCode
    pure ()
  pure ([], state)

riverSpawnPrelude :: String
riverSpawnPrelude =
  "XDG_RUNTIME_DIR=\"${XDG_RUNTIME_DIR:-/run/user/$(id -u)}\"; "
    ++ "export XDG_RUNTIME_DIR; "
    ++ "if [ -z \"${WAYLAND_DISPLAY:-}\" ]; then "
    ++ "for socket in \"$XDG_RUNTIME_DIR\"/wayland-*; do "
    ++ "[ -S \"$socket\" ] || continue; "
    ++ "WAYLAND_DISPLAY=\"$(basename \"$socket\")\"; "
    ++ "break; "
    ++ "done; "
    ++ "fi; "
    ++ "export WAYLAND_DISPLAY=\"${WAYLAND_DISPLAY:-wayland-1}\"; "
    ++ "export XDG_CURRENT_DESKTOP=river; "
    ++ "export XDG_SESSION_DESKTOP=river-xmonad; "
    ++ "export XDG_SESSION_TYPE=wayland; "
    ++ "export IMALISON_SESSION_TYPE=wayland; "
    ++ "export IMALISON_WINDOW_MANAGER=river-xmonad; "

configLog :: String -> IO ()
configLog message = do
  putStrLn $ "imalison-river-xmonad: " ++ message
  hFlush stdout

layoutAction
  :: (LayoutClass l Window, Read (l Window), Message message)
  => message
  -> RiverWMWaylandAction l
layoutAction = handleRiverWMLayoutMessage

stackAction
  :: (W.StackSet WorkspaceId (l Window) Window RiverWMOutputId ScreenDetail
      -> W.StackSet WorkspaceId (l Window) Window RiverWMOutputId ScreenDetail)
  -> RiverWMWaylandAction l
stackAction f state =
  pure $ modifyRiverWMStackSet f state

stackActionWarpPointer
  :: (W.StackSet WorkspaceId (l Window) Window RiverWMOutputId ScreenDetail
      -> W.StackSet WorkspaceId (l Window) Window RiverWMOutputId ScreenDetail)
  -> RiverWMWaylandAction l
stackActionWarpPointer f state =
  pure $ modifyRiverWMStackSetAndWarpPointer f state

data ScratchpadDefinition = ScratchpadDefinition
  { scratchpadName :: !String
  , scratchpadCommand :: !String
  , scratchpadMatches :: !(RiverWMWindowState -> Bool)
  }

ordinaryWorkspaces :: [WorkspaceId]
ordinaryWorkspaces = map show [(1 :: Int) .. 9]

minimizedWorkspace :: WorkspaceId
minimizedWorkspace = "__minimized"

specialWorkspaces :: [WorkspaceId]
specialWorkspaces =
  minimizedWorkspace : map (scratchpadWorkspace . scratchpadName) scratchpadDefinitions

scratchpadWorkspace :: String -> WorkspaceId
scratchpadWorkspace name = "__scratchpad:" ++ name

isSpecialWorkspace :: WorkspaceId -> Bool
isSpecialWorkspace workspace =
  workspace == minimizedWorkspace || "__scratchpad:" `isPrefixOf` workspace

scratchpadDefinitions :: [ScratchpadDefinition]
scratchpadDefinitions =
  [ ScratchpadDefinition "element" "element-desktop" $
      anyMatcher [appIdMatches "Element", appIdMatches "element"]
  , ScratchpadDefinition "htop" "ghostty --title=htop -e htop" $
      titleContains "htop"
  , ScratchpadDefinition "slack" "slack" $
      anyMatcher [appIdMatches "Slack", appIdMatches "slack"]
  , ScratchpadDefinition "spotify" "spotify" $
      anyMatcher [appIdMatches "Spotify", appIdMatches "spotify"]
  , ScratchpadDefinition "transmission" "transmission-gtk" $
      anyMatcher [titleContains "Transmission", appIdContains "transmission"]
  , ScratchpadDefinition "volume" "pavucontrol" $
      anyMatcher [appIdMatches "Pavucontrol", appIdContains "pavucontrol"]
  ]

anyMatcher :: [RiverWMWindowState -> Bool] -> RiverWMWindowState -> Bool
anyMatcher matchers windowState =
  any ($ windowState) matchers

appIdMatches :: String -> RiverWMWindowState -> Bool
appIdMatches expected windowState =
  lower expected == maybe "" lower (riverWMWindowAppId windowState)

appIdContains :: String -> RiverWMWindowState -> Bool
appIdContains needle windowState =
  lower needle `isInfixOf` maybe "" lower (riverWMWindowAppId windowState)

titleContains :: String -> RiverWMWindowState -> Bool
titleContains needle windowState =
  lower needle `isInfixOf` maybe "" lower (riverWMWindowTitle windowState)

lower :: String -> String
lower = map toLower

closeFocusedWindow :: RiverWMWaylandAction l
closeFocusedWindow state@RiverWMState{riverWMStackSet, riverWMWindowIds} =
  pure
    ( maybe [] ((: []) . RiverWMCloseWindow) $
        W.peek riverWMStackSet >>= (`M.lookup` riverWMWindowIds)
    , state
    )

minimizeFocusedWindow :: RiverWMWaylandAction l
minimizeFocusedWindow =
  stackAction $ W.shift minimizedWorkspace

restoreLastMinimizedWindow :: RiverWMWaylandAction l
restoreLastMinimizedWindow =
  stackActionWarpPointer $ \stackSet ->
    case workspaceFocusedWindow minimizedWorkspace stackSet of
      Nothing -> stackSet
      Just window ->
        let currentTag = W.currentTag stackSet
        in W.focusWindow window (W.shiftWin currentTag window stackSet)

toggleScratchpad :: String -> RiverWMWaylandAction l
toggleScratchpad name state@RiverWMState{riverWMStackSet} =
  case find ((== name) . scratchpadName) scratchpadDefinitions of
    Nothing ->
      pure ([], state)
    Just scratchpad ->
      case W.peek riverWMStackSet of
        Just focused | focused `elem` matchingWindows ->
          pure $ modifyRiverWMStackSet (W.shift $ scratchpadWorkspace name) state
        _ ->
          case matchingWindows of
            window : _ ->
              pure $ modifyRiverWMStackSetAndWarpPointer (showScratchpadWindow window) state
            [] ->
              spawnAction (scratchpadCommand scratchpad) state
      where
        matchingWindows = scratchpadWindows scratchpad state
        showScratchpadWindow window stackSet =
          let currentTag = W.currentTag stackSet
          in W.float window nearFullScratchpadRect $
            W.focusWindow window (W.shiftWin currentTag window stackSet)

nearFullScratchpadRect :: W.RationalRect
nearFullScratchpadRect =
  W.RationalRect left top width height
  where
    width = 0.9
    height = 0.9
    left = 0.95 - width
    top = 0.95 - height

scratchpadWindows :: ScratchpadDefinition -> RiverWMState l -> [Window]
scratchpadWindows ScratchpadDefinition{scratchpadMatches} RiverWMState{riverWMWindows} =
  [ riverWMWindowXWindow windowState
  | windowState <- M.elems riverWMWindows
  , scratchpadMatches windowState
  ]

selectWindowAction
  :: String
  -> (Window -> RiverWMState l -> ([RiverWMRequest], RiverWMState l))
  -> RiverWMWaylandAction l
selectWindowAction prompt action state = do
  selected <- rofiSelectWindow prompt state
  pure $ maybe ([], state) (`action` state) selected

focusSelectedWindow :: Window -> RiverWMState l -> ([RiverWMRequest], RiverWMState l)
focusSelectedWindow window state =
  modifyRiverWMStackSetAndWarpPointer (focusWindowEverywhere window) state

bringSelectedWindow :: Window -> RiverWMState l -> ([RiverWMRequest], RiverWMState l)
bringSelectedWindow window state =
  modifyRiverWMStackSetAndWarpPointer (bringWindowToCurrentWorkspace window) state

replaceSelectedWindow :: Window -> RiverWMState l -> ([RiverWMRequest], RiverWMState l)
replaceSelectedWindow selected state =
  modifyRiverWMStackSetAndWarpPointer replaceWindow state
  where
    replaceWindow stackSet =
      case (W.peek stackSet, W.findTag selected stackSet) of
        (Just focused, Just selectedWorkspace)
          | focused /= selected ->
              W.focusWindow selected $
                W.shiftWin selectedWorkspace focused $
                  W.shiftWin (W.currentTag stackSet) selected stackSet
        _ -> stackSet

gatherFocusedAppId :: RiverWMWaylandAction l
gatherFocusedAppId state@RiverWMState{riverWMStackSet, riverWMWindowIds, riverWMWindows} =
  pure $ modifyRiverWMStackSet gatherMatching state
  where
    focusedAppId = do
      focused <- W.peek riverWMStackSet
      windowId <- M.lookup focused riverWMWindowIds
      riverWMWindowAppId =<< M.lookup windowId riverWMWindows

    matchingWindows =
      [ riverWMWindowXWindow windowState
      | windowState <- M.elems riverWMWindows
      , riverWMWindowAppId windowState == focusedAppId
      ]

    gatherMatching stackSet =
      case focusedAppId of
        Nothing -> stackSet
        Just _ ->
          foldl' (\acc window -> W.shiftWin (W.currentTag acc) window acc) stackSet matchingWindows

rofiSelectWindow :: String -> RiverWMState l -> IO (Maybe Window)
rofiSelectWindow prompt state =
  case windowEntries state of
    [] ->
      pure Nothing
    entries -> do
      (exitCode, selected, _stderr) <-
        readCreateProcessWithExitCode
          (shell $ "rofi -dmenu -i -show-icons -p " ++ shellQuote prompt)
          (concatMap formatWindowEntry entries)
      pure $ case exitCode of
        ExitSuccess -> parseSelectedWindow selected
        _ -> Nothing

data WindowEntry = WindowEntry
  { windowEntryWindow :: !Window
  , windowEntryWorkspace :: !WorkspaceId
  , windowEntryAppId :: !String
  , windowEntryTitle :: !String
  }

windowEntries :: RiverWMState l -> [WindowEntry]
windowEntries RiverWMState{riverWMStackSet, riverWMWindowIds, riverWMWindows} =
  [ WindowEntry window (W.tag workspace) appId title
  | workspace <- W.workspaces riverWMStackSet
  , not (isSpecialWorkspace $ W.tag workspace)
  , window <- W.integrate' (W.stack workspace)
  , let windowId = M.lookup window riverWMWindowIds
  , Just windowState <- [windowId >>= (`M.lookup` riverWMWindows)]
  , let appId = fromMaybe "window" (riverWMWindowAppId windowState)
        title = fromMaybe "" (riverWMWindowTitle windowState)
  ]

formatWindowEntry :: WindowEntry -> String
formatWindowEntry WindowEntry{..} =
  visibleLabel ++ "\0icon\x1f" ++ iconName ++ "\n"
  where
    visibleLabel =
      show windowEntryWindow
        ++ "\t["
        ++ windowEntryWorkspace
        ++ "] "
        ++ if null windowEntryTitle
          then windowEntryAppId
          else windowEntryAppId ++ " - " ++ windowEntryTitle
    iconName = if null windowEntryAppId then "application-x-executable" else windowEntryAppId

parseSelectedWindow :: String -> Maybe Window
parseSelectedWindow selected =
  case reads (takeWhile (/= '\t') $ takeWhile (/= '\0') selected) of
    (window, _) : _ -> Just window
    [] -> Nothing

focusWindowEverywhere
  :: Eq sid
  => Window
  -> W.StackSet WorkspaceId l Window sid sd
  -> W.StackSet WorkspaceId l Window sid sd
focusWindowEverywhere window stackSet =
  maybe stackSet (\workspace -> W.focusWindow window (W.greedyView workspace stackSet)) $
    W.findTag window stackSet

bringWindowToCurrentWorkspace
  :: Eq sid
  => Window
  -> W.StackSet WorkspaceId l Window sid sd
  -> W.StackSet WorkspaceId l Window sid sd
bringWindowToCurrentWorkspace window stackSet =
  W.focusWindow window (W.shiftWin (W.currentTag stackSet) window stackSet)

workspaceFocusedWindow :: WorkspaceId -> W.StackSet WorkspaceId l Window sid sd -> Maybe Window
workspaceFocusedWindow workspace stackSet =
  W.focus <$> (W.stack =<< find ((== workspace) . W.tag) (W.workspaces stackSet))

shellQuote :: String -> String
shellQuote value =
  "'" ++ concatMap quoteChar value ++ "'"
  where
    quoteChar '\'' = "'\\''"
    quoteChar char = [char]

viewNextEmptyWorkspace :: RiverWMWaylandAction l
viewNextEmptyWorkspace =
  stackAction $ \stackSet ->
    maybe stackSet (`W.greedyView` stackSet) (nextEmptyWorkspace stackSet)

shiftFocusedToNextEmptyWorkspace :: Bool -> RiverWMWaylandAction l
shiftFocusedToNextEmptyWorkspace follow =
  (if follow then stackActionWarpPointer else stackAction) $ \stackSet ->
    maybe stackSet (`shiftFocusedToWorkspace` stackSet) (nextEmptyWorkspace stackSet)
  where
    shiftFocusedToWorkspace workspace stackSet =
      let shifted = W.shift workspace stackSet
      in if follow then W.greedyView workspace shifted else shifted

nextEmptyWorkspace
  :: W.StackSet WorkspaceId l Window sid sd
  -> Maybe WorkspaceId
nextEmptyWorkspace stackSet =
  find (`workspaceIsEmpty` stackSet) candidates
  where
    currentTag = W.currentTag stackSet
    candidates =
      case break (== currentTag) ordinaryWorkspaces of
        (_before, []) -> ordinaryWorkspaces
        (before, _current : after) -> after ++ before

workspaceIsEmpty
  :: WorkspaceId
  -> W.StackSet WorkspaceId l Window sid sd
  -> Bool
workspaceIsEmpty workspace stackSet =
  maybe False (null . W.integrate' . W.stack) $
    find ((== workspace) . W.tag) (W.workspaces stackSet)

directionalSwap :: Direction -> RiverWMWaylandAction l
directionalSwap direction state@RiverWMState{riverWMStackSet} =
  pure $ modifyRiverWMStackSet swapTarget state
  where
    target = directionalTargetAmong (W.index riverWMStackSet) direction state
    swapTarget stackSet =
      maybe (fallbackDirectionalSwap direction stackSet) (`swapFocusedWithWindow` stackSet) target

fallbackDirectionalSwap
  :: Direction
  -> W.StackSet WorkspaceId l Window sid sd
  -> W.StackSet WorkspaceId l Window sid sd
fallbackDirectionalSwap DirectionUp = W.swapUp
fallbackDirectionalSwap DirectionLeft = W.swapUp
fallbackDirectionalSwap DirectionDown = W.swapDown
fallbackDirectionalSwap DirectionRight = W.swapDown

swapFocusedWithWindow
  :: Window
  -> W.StackSet WorkspaceId l Window sid sd
  -> W.StackSet WorkspaceId l Window sid sd
swapFocusedWithWindow target stackSet =
  case W.peek stackSet of
    Just focused | focused /= target ->
      W.modify' (swapStackOrder focused target) stackSet
    _ -> stackSet

swapStackOrder :: Eq a => a -> a -> W.Stack a -> W.Stack a
swapStackOrder focused target stack =
  stackFromListFocused stack focused $
    map swapWindow (W.integrate stack)
  where
    swapWindow window
      | window == focused = target
      | window == target = focused
      | otherwise = window

stackFromListFocused :: Eq a => W.Stack a -> a -> [a] -> W.Stack a
stackFromListFocused fallback focused windows =
  case break (== focused) windows of
    (before, _focused : after) -> W.Stack focused (reverse before) after
    _ -> fallback

focusDirectionalScreen :: Direction -> RiverWMWaylandAction l
focusDirectionalScreen direction =
  stackAction $ \stackSet ->
    maybe stackSet ((`W.view` stackSet) . W.tag . W.workspace) $
      directionalScreenTarget direction stackSet

shiftFocusedToDirectionalScreen :: Bool -> Direction -> RiverWMWaylandAction l
shiftFocusedToDirectionalScreen follow direction =
  (if follow then stackActionWarpPointer else stackAction) $ \stackSet ->
    maybe stackSet (shiftToScreen stackSet) $
      directionalScreenTarget direction stackSet
  where
    shiftToScreen stackSet screen =
      let workspace = W.tag (W.workspace screen)
          shifted = W.shift workspace stackSet
      in if follow then W.view workspace shifted else shifted

shiftFocusedToEmptyWorkspaceOnDirectionalScreen :: Direction -> RiverWMWaylandAction l
shiftFocusedToEmptyWorkspaceOnDirectionalScreen direction =
  stackActionWarpPointer $ \stackSet ->
    maybe stackSet (shiftToEmptyWorkspaceOnScreen stackSet) $
      directionalScreenTarget direction stackSet
  where
    shiftToEmptyWorkspaceOnScreen stackSet screen =
      let workspace = W.tag (W.workspace screen)
          onDestination = W.view workspace (W.shift workspace stackSet)
      in maybe onDestination
        (\emptyWorkspace -> W.greedyView emptyWorkspace (W.shift emptyWorkspace onDestination))
        (nextEmptyWorkspace onDestination)

directionalFocus :: Direction -> RiverWMWaylandAction l
directionalFocus direction state =
  pure $ modifyRiverWMStackSet focusDirectionalWindow state
  where
    focusDirectionalWindow stackSet =
      maybe (fallbackDirectionalFocus direction stackSet) (`W.focusWindow` stackSet) $
        directionalTarget direction state

fallbackDirectionalFocus
  :: Direction
  -> W.StackSet WorkspaceId l Window sid sd
  -> W.StackSet WorkspaceId l Window sid sd
fallbackDirectionalFocus DirectionUp = W.focusUp
fallbackDirectionalFocus DirectionLeft = W.focusUp
fallbackDirectionalFocus DirectionDown = W.focusDown
fallbackDirectionalFocus DirectionRight = W.focusDown

directionalTarget :: Direction -> RiverWMState l -> Maybe Window
directionalTarget direction state@RiverWMState{riverWMStackSet} =
  directionalTargetAmong (W.index riverWMStackSet) direction state

directionalTargetAmong :: [Window] -> Direction -> RiverWMState l -> Maybe Window
directionalTargetAmong allowed direction RiverWMState{riverWMStackSet, riverWMWindows, riverWMWindowIds} = do
  focused <- W.peek riverWMStackSet
  focusedId <- M.lookup focused riverWMWindowIds
  focusedRect <- riverWMWindowDesired =<< M.lookup focusedId riverWMWindows
  let focusedCenter = rectCenter focusedRect
      candidates =
        [ (window, directionScore direction focusedCenter (rectCenter rect))
        | (windowId, RiverWMWindowState{riverWMWindowXWindow = window, riverWMWindowDesired = Just rect}) <-
            M.toList riverWMWindows
        , windowId /= focusedId
        , window `elem` allowed
        ]
      viable = mapMaybe sequenceCandidate candidates
  fst <$> minimumMaybeBy (compare `on` snd) viable

directionalScreenTarget
  :: Direction
  -> W.StackSet WorkspaceId l Window sid ScreenDetail
  -> Maybe (W.Screen WorkspaceId l Window sid ScreenDetail)
directionalScreenTarget direction stackSet =
  fst <$> minimumMaybeBy (compare `on` snd) viable
  where
    focusedCenter = screenCenter (W.current stackSet)
    candidates =
      [ (screen, directionScore direction focusedCenter (screenCenter screen))
      | screen <- W.visible stackSet
      ]
    viable = mapMaybe sequenceCandidate candidates

screenCenter :: W.Screen WorkspaceId l Window sid ScreenDetail -> (Double, Double)
screenCenter = rectCenter . screenRect . W.screenDetail

equalColumnRects :: Rectangle -> Int -> [Rectangle]
equalColumnRects _ count | count <= 0 = []
equalColumnRects rect 1 = [rect]
equalColumnRects (Rectangle x y width height) count =
  [ Rectangle
      (x + fromIntegral riverOuterGap + fromIntegral (columnOffset index))
      (y + fromIntegral riverOuterGap)
      (fromIntegral (columnWidth index))
      contentHeight
  | index <- [0 .. count - 1]
  ]
  where
    totalWidth = max 0 (fromIntegral width - 2 * riverOuterGap - riverInnerGap * (count - 1))
    contentHeight = fromIntegral (max 1 (fromIntegral height - 2 * riverOuterGap :: Int))
    baseWidth = totalWidth `div` count
    extraPixels = totalWidth `mod` count
    columnWidth index = baseWidth + if index < extraPixels then 1 else 0
    columnOffset index = index * baseWidth + min index extraPixels + index * riverInnerGap

riverOuterGap :: Int
riverOuterGap = 10

riverInnerGap :: Int
riverInnerGap = 5

sequenceCandidate :: (a, Maybe b) -> Maybe (a, b)
sequenceCandidate (value, Just score) = Just (value, score)
sequenceCandidate (_, Nothing) = Nothing

rectCenter :: Rectangle -> (Double, Double)
rectCenter (Rectangle x y width height) =
  ( fromIntegral x + fromIntegral width / 2
  , fromIntegral y + fromIntegral height / 2
  )

directionScore :: Direction -> (Double, Double) -> (Double, Double) -> Maybe (Double, Double)
directionScore direction (fx, fy) (cx, cy) =
  case direction of
    DirectionUp | cy < fy -> Just (fy - cy, abs (cx - fx))
    DirectionDown | cy > fy -> Just (cy - fy, abs (cx - fx))
    DirectionLeft | cx < fx -> Just (fx - cx, abs (cy - fy))
    DirectionRight | cx > fx -> Just (cx - fx, abs (cy - fy))
    _ -> Nothing

minimumMaybeBy :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumMaybeBy _ [] = Nothing
minimumMaybeBy compareFn xs = Just (minimumBy compareFn xs)

addHyperChordBindings
  :: RiverWMWaylandModifiers
  -> RiverWMWaylandModifiers
  -> [RiverWMWaylandKeyBinding l]
  -> [RiverWMWaylandKeyBinding l]
addHyperChordBindings hyperMask chordMask bindings =
  bindings ++ M.elems chosen
  where
    existingKeys =
      M.fromList
        [ ((riverWMWaylandKeyModifiers binding, riverWMWaylandKeyKeysym binding), ())
        | binding <- bindings
        ]

    chordBinding binding@RiverWMWaylandKeyBinding{riverWMWaylandKeyModifiers} =
      binding
        { riverWMWaylandKeyModifiers =
            (riverWMWaylandKeyModifiers .&. complement hyperMask) .|. chordMask
        }

    candidates =
      [ ( (riverWMWaylandKeyModifiers chorded, riverWMWaylandKeyKeysym chorded)
        , (score (riverWMWaylandKeyModifiers binding), chorded)
        )
      | binding <- bindings
      , riverWMWaylandKeyModifiers binding .&. hyperMask /= 0
      , let chorded = chordBinding binding
      , M.notMember (riverWMWaylandKeyModifiers chorded, riverWMWaylandKeyKeysym chorded) existingKeys
      ]

    chosen =
      fmap snd $
        foldl' keepBest M.empty candidates

    keepBest selected (bindingKey, candidate@(candidateScore, _binding)) =
      case M.lookup bindingKey selected of
        Nothing -> M.insert bindingKey candidate selected
        Just (bestScore, _) ->
          if candidateScore < bestScore
            then M.insert bindingKey candidate selected
            else selected

    score modifiers =
      length $
        filter (/= 0)
          [ modifiers .&. shift
          , modifiers .&. ctrl
          , modifiers .&. alt
          , modifiers .&. hyper
          , modifiers .&. super
          , modifiers .&. riverWMWaylandModifierMod5
          ]

noMods, shift, ctrl, alt, hyper, super, hyperChord :: RiverWMWaylandModifiers
noMods = riverWMWaylandModifierNone
shift = riverWMWaylandModifierShift
ctrl = riverWMWaylandModifierCtrl
alt = riverWMWaylandModifierAlt
hyper = riverWMWaylandModifierHyper
super = riverWMWaylandModifierSuper
hyperChord = ctrl .|. alt .|. super
