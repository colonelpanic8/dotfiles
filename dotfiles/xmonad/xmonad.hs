{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances,
             MultiParamTypeClasses, ExistentialQuantification,
             FlexibleInstances, FlexibleContexts #-}
module Main where

import qualified Control.Arrow as A
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.MultiMap as MM
import           Graphics.X11.ExtraTypes.XF86
import           Network.HostName
import           System.Directory
import           System.FilePath.Posix
import           System.Taffybar.Hooks.PagerHints
import           Text.Printf

import           XMonad hiding ( (|||) )
import           XMonad.Actions.CycleWS hiding (nextScreen)
import qualified XMonad.Actions.DynamicWorkspaceOrder as DWO
import           XMonad.Actions.DynamicWorkspaces hiding (withWorkspace)
import           XMonad.Actions.Minimize
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowBringer
import           XMonad.Actions.WindowGo
import           XMonad.Actions.WorkspaceNames
import           XMonad.Config ()
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize
import           XMonad.Layout.Accordion
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.Cross
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.MagicFocus
import           XMonad.Layout.Minimize
import           XMonad.Layout.MultiColumns
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import qualified XMonad.Layout.Renamed as RN
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
import           XMonad.Util.CustomKeys
import qualified XMonad.Util.Dmenu as DM
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.Minimize
import           XMonad.Util.NamedScratchpad
    (NamedScratchpad(NS), nonFloating, namedScratchpadAction)
import           XMonad.Util.NamedWindows (getName)

main =
  xmonad . docks . pagerHints . ewmh $
  def
  { modMask = mod4Mask
  , terminal = "urxvt"
  , manageHook = myManageHook <+> manageHook def
  , layoutHook = myLayoutHook
  , borderWidth = 0
  , normalBorderColor = "#000000"
  , focusedBorderColor = "#455a64"
  , logHook =
      updatePointer (0.5, 0.5) (0, 0) +++
      toggleFadeInactiveLogHook 0.9
  , handleEventHook =
      fullscreenEventHook +++ followIfNoMagicFocus +++ minimizeEventHook
  , startupHook = myStartup
  , keys = customKeys (const []) addKeys
  }
  where
    x +++ y = mappend y x

-- Utility functions

(<..>) a b = (fmap . fmap) a b

forkM :: Monad m => (i -> m a) -> (i -> m b) -> i -> m (a, b)
forkM a b input = do
  resA <- a input
  resB <- b input
  return (resA, resB)

tee :: Monad m => (i -> m a) -> (i -> m b) -> i -> m a
tee = (fmap . fmap . fmap) (fmap fst) forkM

(>>=/) :: Monad m => m a -> (a -> m b) -> m a
(>>=/) a = (a >>=) . tee return

findM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM f = runMaybeT . msum . map (MaybeT . f)

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

ifL :: a -> a -> Bool -> a
ifL a b c = if' c a b

toggleInMap' :: Ord k => Bool -> k -> M.Map k Bool -> M.Map k Bool
toggleInMap' d k m =
  let existingValue = M.findWithDefault d k m
  in M.insert k (not existingValue) m

toggleInMap :: Ord k => k -> M.Map k Bool -> M.Map k Bool
toggleInMap = toggleInMap' True

maybeRemap k = M.findWithDefault k k

(<$.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<$.>) l r = fmap l . r

withFocusedR f = withWindowSet (f . W.peek)

withFocusedD d f = maybe (return d) f <$> withWindowSet (return . W.peek)

withWorkspaceR f = withWindowSet $ f . W.workspace . W.current

mapP = mapP' id

mapP' f f' = map (f A.&&& f')

minimizedWindows = withMinimized return

visibleWindows = (\\) <$> (withWorkspaceR $ return . W.integrate' . W.stack) <*> minimizedWindows

-- Selectors

isHangoutsTitle = isPrefixOf "Google Hangouts"
isChromeClass = isInfixOf "chrome"
chromeSelectorBase = isChromeClass <$> className

chromeSelector = chromeSelectorBase <&&> (not . isHangoutsTitle) <$> title
spotifySelector = className =? "Spotify"
emacsSelector = className =? "Emacs"
transmissionSelector = fmap (isPrefixOf "Transmission") title
hangoutsSelector = chromeSelectorBase <&&> fmap isHangoutsTitle title
volumeSelector = className =? "Pavucontrol"
keepassSelector = className =? "keepassxc"

virtualClasses =
  [ (hangoutsSelector, "Hangouts")
  , (chromeSelector, "Chrome")
  , (transmissionSelector, "Transmission")
  ]

-- Commands

hangoutsCommand = "start_hangouts.sh"
spotifyCommand = "spotify"
chromeCommand = "google-chrome-unstable"
emacsCommand = "emacsclient -c"
htopCommand = "urxvt -e htop"
transmissionCommand = "transmission-gtk"
volumeCommand = "pavucontrol"
keepassCommand = "systemctl --user restart keepassx.service"
taffybarCommand = "restart_taffybar.sh"

-- Startup hook

tvScreenId :: ScreenId
tvScreenId = 1

disableTVFading = setFading (Just tvScreenId) False

hostNameToAction =
  M.fromList [ ("imalison-arch", disableTVFading)
             , ("imalison-uber-loaner", return ())
             ]

myStartup = do
  spawn "systemctl --user start wm.target"
  hostName <- io getHostName
  M.findWithDefault (return ()) hostName hostNameToAction

-- Manage hook

myManageHook =
  composeOne
  [ isFullscreen -?> doFullFloat
    -- [transmissionSelector --> doShift "5"]
    -- Hangouts being on a separate workspace freezes chrome
    -- , [ hangoutsSelector --> doShift "2"]
  ]

-- Toggles

unmodifyLayout (ModifiedLayout _ x') =  x'

selectLimit =
  DM.menuArgs "rofi" ["-dmenu", "-i"] ["2", "3", "4"] >>= (setLimit . read)

data MyToggles
  = LIMIT
  | GAPS
  | MAGICFOCUS
  deriving (Read, Show, Eq, Typeable)

instance Transformer MyToggles Window where
  transform LIMIT x k = k (limitSlice 2 x) unmodifyLayout
  transform GAPS x k = k (smartSpacing 5 x) unmodifyLayout
  transform MAGICFOCUS x k = k (magicFocus x) unmodifyLayout

myToggles = [LIMIT, GAPS, MAGICFOCUS]
otherToggles = [NBFULL, MIRROR]

followIfNoMagicFocus =
  followOnlyIf $ maybe False not <$> isToggleActive MAGICFOCUS

togglesMap =
  fmap M.fromList $ sequence $
  map toggleTuple myToggles ++ map toggleTuple otherToggles
  where
    toggleTuple toggle =
      fmap (\str -> (str, Toggle toggle)) (toggleToStringWithState toggle)


toggleStateToString s =
  case s of
    Just True -> "ON"
    Just False -> "OFF"
    Nothing -> "N/A"

toggleToStringWithState :: (Transformer t Window, Show t) => t -> X String
toggleToStringWithState toggle =
  printf "%s (%s)" (show toggle) . toggleStateToString <$>
  isToggleActive toggle

selectToggle =
  togglesMap >>= DM.menuMapArgs "rofi" ["-dmenu", "-i"] >>=
             flip whenJust sendMessage

toggleInState :: (Transformer t Window) => t -> Maybe Bool -> X Bool
toggleInState t s = fmap (/= s) (isToggleActive t)

setToggleActive' toggle active =
  toggleInState toggle (Just active) >>=/
  flip when (sendMessage $ Toggle toggle)

-- Ambiguous type reference without signature
setToggleActive :: (Transformer t Window) => t -> Bool -> X ()
setToggleActive = (void .) . setToggleActive'

deactivateFull = setToggleActive NBFULL False

toggleOr toggle toState action =
  setToggleActive' toggle toState >>= ((`when` action) . not)

deactivateFullOr = toggleOr NBFULL False
deactivateFullAnd action = sequence_ [deactivateFull, action]

andDeactivateFull action = sequence_ [action, deactivateFull]

goFullscreen = sendMessage $ Toggle NBFULL

-- Layout setup

myTabConfig =
  def { activeBorderColor = "#66cccc" }

rename newName = RN.renamed [RN.Replace newName]

layoutsStart layout = (layout, [Layout layout])
(|||!) (joined, layouts) newLayout =
    (joined ||| newLayout, layouts ++ [Layout newLayout])

layoutInfo =
  layoutsStart (rename "Columns" $ multiCol [1, 1] 2 0.01 (-0.5)) |||!
  rename "Large Main" (Tall 1 (3 / 100) (3 / 4)) |||!
  rename "2 Columns" (Tall 1 (3 / 100) (1 / 2)) |||!
  Accordion |||! simpleCross |||! myTabbed
    where
      myTabbed = tabbed shrinkText myTabConfig

layoutList = snd layoutInfo

layoutNames = [description layout | layout <- layoutList]

selectLayout =
  DM.menuArgs "rofi" ["-dmenu", "-i"] layoutNames >>=
  (sendMessage . JumpToLayout)

myLayoutHook =
  avoidStruts . minimize . boringAuto . mkToggle1 MIRROR . mkToggle1 LIMIT .
  mkToggle1 GAPS . mkToggle1 MAGICFOCUS . mkToggle1 NBFULL . workspaceNamesHook .
  lessBorders Screen $ fst layoutInfo

-- WindowBringer

myWindowBringerConfig =
  def { menuCommand = "rofi"
      , menuArgs = ["-dmenu", "-i"]
      , windowTitler = myDecorateName
      }

classIfMatches window entry =
  if' <$> runQuery (fst entry) window <*>
      pure (Just $ snd entry) <*>
      pure Nothing

getClassRaw w = fmap resClass $ withDisplay $ io . flip getClassHint w

getVirtualClass = flip findM virtualClasses . classIfMatches

getClass w = fromMaybe <$> getClassRaw w <*> getVirtualClass w

myDecorateName ws w = do
  name <- show <$> getName w
  classTitle <- getClass w
  workspaceToName <- getWorkspaceNames
  return $ printf "%-20s%-40s %+30s" classTitle (take 40 name)
           "in " ++ workspaceToName (W.tag ws)

-- This needs access to X in order to unminimize, which means that I can't be
-- done with the existing window bringer interface
myWindowAct  c@WindowBringerConfig { menuCommand = cmd
                                   , menuArgs = args
                                   } action =
  do
    visible <- visibleWindows
    windowMap' c { windowFilter = not . flip elem visible } >>=
               DM.menuMapArgs cmd args >>= flip whenJust action


myBringWindow window =
  sequence_ [ maximizeWindow window
            , windows $ W.focusWindow window . bringWindow window
            ]

-- Dynamic Workspace Renaming

windowClassFontAwesomeFile =
  fmap (</> ".lib/resources/window_class_to_fontawesome.json") getHomeDirectory

getClassRemap =
  fmap (fromMaybe M.empty . decode) $
       windowClassFontAwesomeFile >>= B.readFile

getClassRemapF = flip maybeRemap <$> getClassRemap
getWSClassNames' w = mapM getClass $ W.integrate' $ W.stack w
getWSClassNames w = io (fmap map getClassRemapF) <*> getWSClassNames' w
currentWSName ws = fromMaybe "" <$> (getWorkspaceNames' <*> pure (W.tag ws))
desiredWSName = (intercalate "|" <$>) . getWSClassNames

setWorkspaceNameToFocusedWindow workspace = do
  currentName <- currentWSName workspace
  newName <- desiredWSName workspace
  when (currentName /= newName) $ setWorkspaceName (W.tag workspace) newName

setWorkspaceNames =
  gets windowset >>= mapM_ setWorkspaceNameToFocusedWindow . W.workspaces

data WorkspaceNamesHook a = WorkspaceNamesHook deriving (Show, Read)

instance LayoutModifier WorkspaceNamesHook Window where
    hook _ = setWorkspaceNames

workspaceNamesHook = ModifiedLayout WorkspaceNamesHook

-- Toggleable fade

newtype ToggleFade a =
  ToggleFade { fadesMap :: M.Map a Bool }
  deriving (Typeable, Read, Show)

instance (Typeable a, Read a, Show a, Ord a) => ExtensionClass (ToggleFade a) where
  initialValue = ToggleFade M.empty
  extensionType = PersistentExtension

fadeEnabledFor query =
  M.findWithDefault True <$> query <*> liftX (fadesMap <$> XS.get)

fadeEnabledForWindow = fadeEnabledFor ask
fadeEnabledForWorkspace = fadeEnabledFor getWindowWorkspace
fadeEnabledForScreen = fadeEnabledFor getWindowScreen

getScreens = withWindowSet $ return . W.screens
getWindowWorkspace' = W.findTag <$> ask <*> liftX (withWindowSet return)
getWindowWorkspace = flip fromMaybe <$> getWindowWorkspace' <*> pure "1"
getWorkspaceToScreen = M.fromList . mapP' (W.tag . W.workspace) W.screen <$> getScreens
getWindowScreen = M.lookup <$> getWindowWorkspace <*> liftX getWorkspaceToScreen
getCurrentScreen = join (withFocusedD Nothing (runQuery getWindowScreen))

fadeCondition =
  isUnfocused <&&> fadeEnabledForWindow <&&>
  fadeEnabledForWorkspace <&&> fadeEnabledForScreen

toggleFadeInactiveLogHook = fadeOutLogHook . fadeIf fadeCondition

toggleFadingForActiveWindow = withWindowSet $
  maybe (return ()) toggleFading . W.peek

toggleFadingForActiveWorkspace =
  withWindowSet $ \ws -> toggleFading $ W.currentTag ws

toggleFadingForActiveScreen = getCurrentScreen >>= toggleFading

toggleFading w = setFading' $ toggleInMap w

setFading w f = setFading' $ M.insert w f

setFading' f =
  fmap (ToggleFade . f . fadesMap) XS.get >>= XS.put

-- Minimize not in class

restoreFocus action =
  withFocused $ \orig -> action >> windows (W.focusWindow orig)

getCurrentWS = W.stack . W.workspace . W.current

withWorkspace f = withWindowSet $ \ws -> maybe (return ()) f (getCurrentWS ws)

currentWS = withWindowSet $ return . getCurrentWS

workspaceWindows = maybe [] W.integrate <$> currentWS

getMinMaxWindows =
  partition <$> (flip elem <$> minimizedWindows) <*> workspaceWindows

maximizedWindows = fmap snd getMinMaxWindows

maximizedOtherClass =
  intersect <$> maximizedWindows <*>
  (currentWS >>= maybe (return []) windowsWithUnfocusedClass)

minimizedSameClass =
  intersect <$> minimizedWindows <*>
  (currentWS >>= maybe (return []) windowsWithFocusedClass)

getClassMatchesWindow w = (==) <$> getClass w
getClassMatchesCurrent = join $ withFocusedD (`seq` False) getClassMatchesWindow

minimizeOtherClassesInWorkspace =
  actOnWindowsInWorkspace minimizeWindow windowsWithUnfocusedClass
maximizeSameClassesInWorkspace =
  actOnWindowsInWorkspace maybeUnminimize windowsWithFocusedClass

-- Type annotation is needed to resolve ambiguity
actOnWindowsInWorkspace :: (Window -> X ())
                        -> (W.Stack Window -> X [Window])
                        -> X ()
actOnWindowsInWorkspace windowAction getWindowsAction = restoreFocus $
  withWorkspace (getWindowsAction >=> mapM_ windowAction)

windowsWithUnfocusedClass ws = windowsWithOtherClasses (W.focus ws) ws
windowsWithFocusedClass ws = windowsWithSameClass (W.focus ws) ws
windowsWithOtherClasses = windowsMatchingClassPredicate (/=)
windowsWithSameClass = windowsMatchingClassPredicate (==)

windowsMatchingClassPredicate predicate window workspace =
  windowsSatisfyingPredicate workspace $ do
    windowClass <- getClass window
    return $ predicate windowClass

windowsSatisfyingPredicate workspace getPredicate = do
  predicate <- getPredicate
  filterM (\w -> predicate <$> getClass w) (W.integrate workspace)

getMatchingUnmatching =
  partition <$> ((. snd) <$> getClassMatchesCurrent) <*> getWindowClassPairs

getWindowClassPairs = join $ mapM windowToClassPair <$> workspaceWindows

windowToClassPair w = (,) w <$> getClass w

windowIsMinimized w = do
  minimized <- XS.gets minimizedStack
  return $ w `elem` minimized

maybeUnminimize w = windowIsMinimized w >>= flip when (maximizeWindow w)

maybeUnminimizeFocused = withFocused maybeUnminimize

maybeUnminimizeAfter = (>> maybeUnminimizeFocused)

maybeUnminimizeClassAfter = (>> maximizeSameClassesInWorkspace)

sameClassOnly action =
  action >> minimizeOtherClassesInWorkspace >> maximizeSameClassesInWorkspace

restoreAll = mapM_ maximizeWindow

restoreAllMinimized = minimizedWindows >>= restoreAll

restoreOrMinimizeOtherClasses = null <$> maximizedOtherClass >>=
  ifL restoreAllMinimized minimizeOtherClassesInWorkspace

restoreThisClassOrMinimizeOtherClasses = minimizedSameClass >>= \ws ->
  if' (null ws) minimizeOtherClassesInWorkspace $ restoreAll ws

getClassPair w = flip (,) w <$> getClass w

windowClassPairs = withWindowSet $ mapM getClassPair . W.allWindows
classToWindowMap = MM.fromList <$> windowClassPairs
allClasses = sort . MM.keys <$> classToWindowMap
thisClass = withWindowSet $ sequence . (getClass <$.> W.peek)

nextClass = do
  classes <- allClasses
  current <- thisClass
  let index = join $ elemIndex <$> current <*> pure classes
  return $ fmap (\i -> cycle classes !! (i + 1)) index

classWindow c = do
  m <- classToWindowMap
  return $ join $ listToMaybe <$> (flip MM.lookup m <$> c)

nextClassWindow = nextClass >>= classWindow

focusNextClass' = join $ windows . maybe id greedyFocusWindow <$> nextClassWindow
focusNextClass = sameClassOnly focusNextClass'

selectClass = join $ DM.menuArgs "rofi" ["-dmenu", "-i"] <$> allClasses

-- Window switching

-- Use greedyView to switch to the correct workspace, and then focus on the
-- appropriate window within that workspace.
greedyFocusWindow w ws =
  W.focusWindow w $
  W.greedyView (fromMaybe (W.currentTag ws) $ W.findTag w ws) ws

shiftThenView i = W.greedyView i . W.shift i

greedyBringWindow w = greedyFocusWindow w . bringWindow w

shiftToEmptyAndView =
  doTo Next EmptyWS DWO.getSortByOrder (windows . shiftThenView)

setFocusedScreen :: ScreenId -> WindowSet -> WindowSet
setFocusedScreen to ws =
  maybe ws (`setFocusedScreen'` ws) $ find ((to ==) . W.screen) (W.visible ws)

setFocusedScreen' to ws @ W.StackSet
  { W.current = prevCurr
  , W.visible = visible
  } = ws { W.current = to
         , W.visible = prevCurr:deleteBy screenEq to visible
         }

  where screenEq a b = W.screen a == W.screen b

nextScreen ws @ W.StackSet { W.visible = visible } =
  case visible of
    next:_ -> setFocusedScreen (W.screen next) ws
    _ -> ws

viewOtherScreen ws = W.greedyView ws . nextScreen

shiftThenViewOtherScreen ws w = viewOtherScreen ws . W.shiftWin ws w

shiftCurrentToWSOnOtherScreen ws s =
  fromMaybe s (flip (shiftThenViewOtherScreen ws) s <$> W.peek s)

shiftToEmptyNextScreen =
  doTo Next EmptyWS DWO.getSortByOrder $ windows . shiftCurrentToWSOnOtherScreen

swapFocusedWith w ws = W.modify' (swapFocusedWith' w) (W.delete' w ws)

swapFocusedWith' w (W.Stack current ls rs) = W.Stack w ls (rs ++ [current])

swapMinimizeStateAfter action =
  withFocused $ \originalWindow -> do
    _ <- action
    restoreFocus $ do
      maybeUnminimizeFocused
      withFocused $ \newWindow ->
        when (newWindow /= originalWindow) $ minimizeWindow originalWindow

-- Named Scratchpads

scratchpads =
  [ NS "htop" htopCommand (title =? "htop") nonFloating
  , NS "spotify" spotifyCommand spotifySelector nonFloating
  , NS "hangouts" hangoutsCommand hangoutsSelector nonFloating
  , NS "volume" volumeCommand volumeSelector nonFloating
  , NS "keepass" keepassCommand keepassSelector nonFloating
  ]

-- TODO: This doesnt work well with minimized windows
doScratchpad =
  maybeUnminimizeAfter . deactivateFullAnd . namedScratchpadAction scratchpads

-- Raise or spawn

myRaiseNextMaybe =
  ((deactivateFullAnd . maybeUnminimizeAfter) .) .
  raiseNextMaybeCustomFocus greedyFocusWindow

myBringNextMaybe =
  ((deactivateFullAnd . maybeUnminimizeAfter) .) .
  raiseNextMaybeCustomFocus greedyBringWindow

bindBringAndRaise :: KeyMask -> KeySym -> X () -> Query Bool -> [((KeyMask, KeySym), X ())]
bindBringAndRaise mask sym start query =
    [ ((mask, sym), doRaiseNext)
    , ((mask .|. controlMask, sym), myBringNextMaybe start query)
    , ((mask .|. shiftMask, sym), doRaiseNext)
    ]
  where doRaiseNext = myRaiseNextMaybe start query

bindBringAndRaiseMany :: [(KeyMask, KeySym, X (), Query Bool)] -> [((KeyMask, KeySym), X())]
bindBringAndRaiseMany = concatMap (\(a, b, c, d) -> bindBringAndRaise a b c d)

-- Screen shift

shiftToNextScreen ws =
  case W.visible ws of
    W.Screen i _ _:_ -> W.view (W.tag i) $ W.shift (W.tag i) ws
    _ -> ws

shiftToNextScreenX = windows shiftToNextScreen

goToNextScreen ws =
  case W.visible ws of
    newVisible:rest -> ws { W.current = newVisible
                          , W.visible = W.current ws:rest
                          }
    _ -> ws

goToNextScreenX = windows goToNextScreen

-- Key bindings

addKeys conf@XConfig { modMask = modm } =

    -- Specific program spawning
    bindBringAndRaiseMany
    [ (modalt, xK_e, spawn emacsCommand, emacsSelector)
    , (modalt, xK_c, spawn chromeCommand, chromeSelector)
    , (modalt, xK_t, spawn transmissionCommand, transmissionSelector)
    ] ++

    -- ScratchPads
    [ ((modalt, xK_m), doScratchpad "htop")
    , ((modalt, xK_v), doScratchpad "volume")
    , ((modalt, xK_h), doScratchpad "hangouts")
    , ((modalt, xK_s), doScratchpad "spotify")
    , ((modalt, xK_k), doScratchpad "keepass")
    , ((modalt .|. controlMask, xK_h),
       myRaiseNextMaybe (spawn hangoutsCommand) hangoutsSelector)
    , ((modalt .|. controlMask, xK_s),
       myRaiseNextMaybe (spawn spotifyCommand) spotifySelector)

    -- Specific program spawning

    , ((modm, xK_p), spawn "rofi -show drun")
    , ((modm .|. shiftMask, xK_p), spawn "rofi -show run")

    -- Window manipulation

    , ((modm, xK_g), andDeactivateFull . maybeUnminimizeAfter $
       myWindowAct myWindowBringerConfig $ windows . greedyFocusWindow)
    , ((modm .|. shiftMask, xK_g), andDeactivateFull . sameClassOnly $
       actionMenu myWindowBringerConfig greedyFocusWindow)
    , ((modm, xK_b), andDeactivateFull $ myWindowAct myWindowBringerConfig myBringWindow)
    , ((modm .|. shiftMask, xK_b),
       swapMinimizeStateAfter $ myWindowAct myWindowBringerConfig $ windows . swapFocusedWith)
    , ((modm .|. controlMask, xK_space), goFullscreen)
    , ((modm, xK_m), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_m),
       deactivateFullOr $ withLastMinimized maximizeWindowAndFocus)
    , ((modm, xK_x), addHiddenWorkspace "NSP" >> (windows $ W.shift "NSP"))
    , ((modalt, xK_space), deactivateFullOr restoreOrMinimizeOtherClasses)
    , ((modalt, xK_Return), deactivateFullAnd restoreAllMinimized)

    -- Focus/Layout manipulation

    , ((modm, xK_s), swapNextScreen >> goToNextScreenX)
    , ((modm, xK_e), goToNextScreenX)
    , ((modm, xK_slash), sendMessage $ Toggle MIRROR)
    , ((modm, xK_backslash), toggleWS)
    , ((modm, xK_space), deactivateFullOr $ sendMessage NextLayout)
    , ((modm, xK_z), shiftToNextScreenX)
    , ((modm .|. shiftMask, xK_z), shiftToEmptyNextScreen)
    , ((modm .|. shiftMask, xK_h), shiftToEmptyAndView)

    -- These need to be rebound to support boringWindows
    , ((modm, xK_j), focusDown)
    , ((modm, xK_k), focusUp)
    , ((modm, xK_m), focusMaster)
    , ((modm, xK_Tab), focusNextClass)
    , ((hyper, xK_e), moveTo Next EmptyWS)

    -- Miscellaneous XMonad

    , ((hyper, xK_1), toggleFadingForActiveWindow)
    , ((hyper .|. shiftMask, xK_1), toggleFadingForActiveWorkspace)
    , ((hyper .|. controlMask, xK_1), toggleFadingForActiveScreen)
    , ((hyper, xK_t), selectToggle)
    , ((modalt, xK_4), selectLimit)
    , ((hyper, xK_3), addWorkspacePrompt def)
    , ((modalt, xK_3), selectWorkspace def)
    , ((hyper .|. mod1Mask, xK_3), removeWorkspace)

    -- Non-XMonad

    , ((modm .|. controlMask, xK_t), spawn taffybarCommand)
    , ((modm, xK_v), spawn "copyq paste")
    , ((modm .|. controlMask, xK_s), spawn "split_out.sh")
    , ((hyper, xK_v), spawn "copyq_rofi.sh")
    , ((hyper, xK_p), spawn "rofi-pass")
    , ((hyper, xK_h), spawn "screenshot.sh")
    , ((hyper, xK_c), spawn "shell_command.sh")
    , ((hyper .|. shiftMask, xK_l), spawn "dm-tool lock")
    , ((hyper, xK_l), selectLayout)
    , ((hyper, xK_k), spawn "rofi_kill_process.sh")
    , ((hyper .|. shiftMask, xK_k),
       spawn "rofi_kill_all.sh")
    , ((hyper, xK_r), spawn "rofi_systemd.sh")
    , ((hyper, xK_0), spawn "tvpower.js")
    , ((modalt, xK_w), spawn "rofi_wallpaper.sh")
    , ((modalt, xK_z), spawn "split_out_chrome_tab.sh")
    , ((hyper, xK_9), spawn "start_synergy.sh")
    , ((hyper, xK_8), spawn "rofi_paswitch.sh")

    -- Media keys

    -- playerctl
    , ((hyper, xK_f), spawn "playerctl play-pause")
    , ((0, xF86XK_AudioPause), spawn "playerctl play-pause")
    , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((hyper, xK_d), spawn "playerctl next")
    , ((0, xF86XK_AudioNext), spawn "playerctl next")
    , ((hyper, xK_a), spawn "playerctl previous")
    , ((0, xF86XK_AudioPrev), spawn "playerctl previous")

    -- Volume control
    , ((0, xF86XK_AudioRaiseVolume), spawn "set_volume.sh up")
    , ((0, xF86XK_AudioLowerVolume), spawn "set_volume.sh down")
    , ((0, xF86XK_AudioMute), spawn "set_volume.sh mute")
    , ((hyper, xK_w), spawn "set_volume.sh up")
    , ((hyper, xK_s), spawn "set_volume.sh down")

    , ((0, xF86XK_MonBrightnessUp), spawn "show_brightness.sh")
    , ((0, xF86XK_MonBrightnessDown), spawn "show_brightness.sh")

    ] ++

    -- Replace moving bindings

    [((additionalMask .|. modm, key), windows $ function workspace)
         | (workspace, key) <- zip (workspaces conf) [xK_1 .. xK_9]
         , (function, additionalMask) <-
             [ (W.greedyView, 0)
             , (W.shift, shiftMask)
             , (shiftThenView, controlMask)
             ]
    ]
    where
      modalt = modm .|. mod1Mask
      hyper = mod3Mask
      hctrl = hyper .|. controlMask

-- Local Variables:
-- flycheck-ghc-args: ("-Wno-missing-signatures")
-- haskell-indent-offset: 2
-- End:
