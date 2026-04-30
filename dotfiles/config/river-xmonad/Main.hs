{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (forkIO)
import Data.Bits ((.&.), complement)
import Data.Function (on)
import Data.List (minimumBy)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Graphics.X11.ExtraTypes.XF86
import System.IO (hFlush, stdout)
import System.Process (spawnCommand, waitForProcess)
import XMonad
import XMonad.Layout.Accordion
import XMonad.Layout.Cross
import XMonad.Layout.Grid
import XMonad.Layout.MultiColumns
import qualified XMonad.Layout.Renamed as RN
import XMonad.River.WindowManager
import XMonad.River.WindowManager.Wayland
import qualified XMonad.StackSet as W

data Direction = DirectionUp | DirectionDown | DirectionLeft | DirectionRight
  deriving (Eq, Show)

main :: IO ()
main = do
  let bindings = keyBindings
  configLog $ "starting imalison-river-xmonad with keybindings=" ++ show (length bindings)
  initialState <- initialRiverWMState (defaultRiverWMConfig riverLayouts)
  runRiverWMWaylandConfig
    RiverWMWaylandConfig
      { riverWMWaylandInitialState = initialState
      , riverWMWaylandKeyBindings = bindings
      }

riverLayouts =
  renamed "4 Columns" (multiCol [1, 1, 1] 2 0.0 (-0.5))
  ||| renamed "3 Columns" (multiCol [1, 1] 2 0.01 (-0.5))
  ||| renamed "Grid" Grid
  ||| renamed "Large Main" (Tall 1 (3 / 100) (3 / 4))
  ||| renamed "2 Columns" (Tall 1 (3 / 100) (1 / 2))
  ||| renamed "Mirror 2 Columns" (Mirror (Tall 1 (3 / 100) (1 / 2)))
  ||| renamed "Accordion" Accordion
  ||| renamed "Cross" simpleCross
  ||| Full
  where
    renamed name = RN.renamed [RN.Replace name]

keyBindings
  :: (LayoutClass l Window, Read (l Window))
  => [RiverWMWaylandKeyBinding l]
keyBindings =
  addHyperChordBindings hyper hyperChord $
    concat
      [ directionalBindings super directionalFocus
      , directionalBindings (super .|. shift) directionalSwap
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
  [ key (mods .|. super) keysym (stackAction $ command workspace)
  | (workspace, keysym) <- zip (map show [(1 :: Int) .. 9]) [xK_1 .. xK_9]
  , (command, mods) <-
      [ (W.greedyView, noMods)
      , (W.shift, shift)
      , (\workspaceId stackSet -> W.greedyView workspaceId (W.shift workspaceId stackSet), ctrl)
      ]
  ]

layoutBindings
  :: (LayoutClass l Window, Read (l Window))
  => [RiverWMWaylandKeyBinding l]
layoutBindings =
  [ key super xK_space (layoutAction NextLayout)
  , key super xK_bracketleft (layoutAction Shrink)
  , key super xK_bracketright (layoutAction Expand)
  , key super xK_comma (layoutAction (IncMasterN 1))
  , key super xK_period (layoutAction (IncMasterN (-1)))
  ]

spawnBindings
  :: [RiverWMWaylandKeyBinding l]
spawnBindings =
  [ key super xK_Return (spawnAction "ghostty --gtk-single-instance=false")
  , key super xK_p (spawnAction "rofi -show drun -show-icons")
  , key (super .|. shift) xK_p (spawnAction "rofi -show run")
  , key (super .|. alt) xK_c (spawnAction "google-chrome-stable")
  , key super xK_e (spawnAction "emacsclient --eval '(emacs-everywhere)'")
  , key super xK_v (spawnAction "wl-paste | wtype -")
  , key hyper xK_v (spawnAction "rofi -modi 'clipboard:greenclip print' -show clipboard")
  , key hyper xK_p (spawnAction "rofi-pass")
  , key hyper xK_h (spawnAction "rofi_shutter")
  , key hyper xK_c (spawnAction "shell_command.sh")
  , key hyper xK_x (spawnAction "rofi_command.sh")
  , key (hyper .|. shift) xK_l (spawnAction "loginctl lock-session")
  , key hyper xK_k (spawnAction "rofi_kill_process.sh")
  , key (hyper .|. shift) xK_k (spawnAction "rofi_kill_all.sh")
  , key hyper xK_r (spawnAction "rofi-systemd")
  , key hyper xK_9 (spawnAction "start_synergy.sh")
  , key hyper xK_backslash (spawnAction "$HOME/dotfiles/dotfiles/lib/functions/mpg341cx_input toggle")
  , key hyper xK_i (spawnAction "rofi_select_input.hs")
  , key hyper xK_o (spawnAction "rofi_paswitch")
  , key hyper xK_w (spawnAction "rofi_wallpaper.sh")
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

directionalSwap :: Direction -> RiverWMWaylandAction l
directionalSwap direction =
  stackAction $
    case direction of
      DirectionUp -> W.swapUp
      DirectionLeft -> W.swapUp
      DirectionDown -> W.swapDown
      DirectionRight -> W.swapDown

directionalFocus :: Direction -> RiverWMWaylandAction l
directionalFocus direction state =
  pure $ modifyRiverWMStackSet focusDirectionalWindow state
  where
    focusDirectionalWindow stackSet =
      maybe stackSet (`W.focusWindow` stackSet) $
        directionalTarget direction state

directionalTarget :: Direction -> RiverWMState l -> Maybe Window
directionalTarget direction RiverWMState{riverWMStackSet, riverWMWindows, riverWMWindowIds} = do
  focused <- W.peek riverWMStackSet
  focusedId <- M.lookup focused riverWMWindowIds
  focusedRect <- riverWMWindowDesired =<< M.lookup focusedId riverWMWindows
  let focusedCenter = rectCenter focusedRect
      candidates =
        [ (window, directionScore direction focusedCenter (rectCenter rect))
        | (windowId, RiverWMWindowState{riverWMWindowXWindow = window, riverWMWindowDesired = Just rect}) <-
            M.toList riverWMWindows
        , windowId /= focusedId
        ]
      viable = mapMaybe sequenceCandidate candidates
  fst <$> minimumMaybeBy (compare `on` snd) viable

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
