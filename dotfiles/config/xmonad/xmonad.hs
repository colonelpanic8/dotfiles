{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import           Codec.Binary.UTF8.String as UTF8
import qualified Codec.Binary.UTF8.String as UTF8String (encode)
import qualified Control.Arrow as A
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.List.Split
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.MultiMap as MM
import           Data.Proxy
import           Data.Tuple.Sequence (sequenceT)
import           Data.Typeable
import           Foreign.C.Types
import           Graphics.X11.ExtraTypes.XF86
import           Network.HostName
import           Safe
import           System.Directory
import           System.Environment.XDG.DesktopEntry
import           System.FilePath.Posix
import           System.IO.Unsafe
import           System.Process
import           Text.Printf
import           Unsafe.Coerce
import           XMonad hiding ( (|||) )
import           XMonad.Actions.CycleWS hiding (nextScreen)
import           XMonad.Actions.CycleWorkspaceByScreen
import qualified XMonad.Actions.DynamicWorkspaceOrder as DWO
import           XMonad.Actions.DynamicWorkspaces hiding (withWorkspace, renameWorkspace)
import           XMonad.Actions.Minimize
import           XMonad.Actions.Navigation2D
import qualified XMonad.Actions.SwapWorkspaces as SW
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowBringer
import           XMonad.Actions.WindowGo
import           XMonad.Actions.WorkspaceNames
import           XMonad.Config ()
import           XMonad.Core (getDirectories)
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.TaffybarPagerHints
import           XMonad.Hooks.WorkspaceHistory
import           XMonad.Layout.Accordion
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.ConditionalModifier
import           XMonad.Layout.Cross
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.MagicFocus
import           XMonad.Layout.Magnifier hiding (Toggle)
import           XMonad.Layout.Minimize
import           XMonad.Layout.MultiColumns
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import qualified XMonad.Layout.Renamed as RN
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Main (launch)
import qualified XMonad.Operations
import qualified XMonad.StackSet as W
import           XMonad.Util.CustomKeys
import qualified XMonad.Util.Dmenu as DM
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.Minimize
import           XMonad.Util.NamedScratchpad as NS
import           XMonad.Util.NamedWindows (getName)
import           XMonad.Util.Run
import           XMonad.Util.Themes
import           XMonad.Util.WorkspaceCompare

myConfig = def
  { modMask = mod4Mask
  , terminal = "alacritty"
  , manageHook
    = namedScratchpadManageHook scratchpads
  , layoutHook = myLayoutHook
  , borderWidth = 0
  , normalBorderColor = "#0096ff"
  , focusedBorderColor = "#ffff00"
  , logHook
  = updatePointer (0.5, 0.5) (0, 0)
  <> toggleFadeInactiveLogHook 0.9
  <> workspaceHistoryHook
  <> setWorkspaceNames
  <> activateLogHook (reader W.focusWindow >>= doF)
  <> logHook def
  , handleEventHook
  =  followIfNoMagicFocus
  <> minimizeEventHook
  <> restartEventHook
  <> myScratchPadEventHook
  , startupHook = myStartup
  , keys = customKeys (const []) addKeys
  }

restartEventHook e@ClientMessageEvent { ev_message_type = mt } = do
  a <- getAtom "XMONAD_RESTART"
  if mt == a
    then XMonad.Operations.restart "imalison-xmonad" True >> return (All True)
    else return $ All True
restartEventHook _ = return $ All True

myNavigation2DConfig = def { defaultTiledNavigation = centerNavigation }

main = do
  dirs <- getDirectories
  (`launch` dirs)
       . docks
       . pagerHints
       . ewmh
       . ewmhFullscreen
       . withNavigation2DConfig myNavigation2DConfig $ myConfig

-- Utility functions

-- Log to a file from anywhere
writeToHomeDirLog stuff = io $ getLogFile >>= flip appendFile (stuff ++ "\n")
  where getLogFile = (</> "temp" </> "xmonad.log") <$> getHomeDirectory

logWindowSet message =
  withWindowSet $ \ws -> writeToHomeDirLog $ printf "%s -- " message $ show ws

xRunCommand cmd = void $ io $ readCreateProcess (shell cmd) ""

(<..>) :: Functor f => (a -> b) -> f (f a) -> f (f b)
(<..>) = fmap . fmap

forkM :: Monad m => (i -> m a) -> (i -> m b) -> i -> m (a, b)
forkM a b = sequenceT . (a A.&&& b)

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

infixl 4 <$$>
(<$$>) :: Functor f => f (a -> b) -> a -> f b
functor <$$> value = ($ value) <$> functor

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

visibleWindows =
  (\\) <$> withWorkspaceR (return . W.integrate' . W.stack)
         <*> minimizedWindows

followingWindow action = do
  orig <- withWindowSet (return . W.peek)
  res <- action
  whenJust orig $ windows . W.focusWindow
  return res

myDmenuArgs = ["-dmenu", "-i", "-show-icons"]

myDmenu = DM.menuArgs "rofi" myDmenuArgs

getWorkspaceDmenu = myDmenu (workspaces myConfig)

-- Selectors

isGmailTitle t = isInfixOf "@gmail.com" t && isInfixOf "Gmail" t
isMessagesTitle = isPrefixOf "Messages"
isChromeClass = isInfixOf "chrome"
noSpecialChromeTitles = helper <$> title
  where helper t = not $ any ($ t) [isGmailTitle, isMessagesTitle]
chromeSelectorBase = isChromeClass <$> className

chromeSelector = chromeSelectorBase <&&> noSpecialChromeTitles
elementSelector = className =? "Element"
emacsSelector = className =? "Emacs"
gmailSelector = chromeSelectorBase <&&> fmap isGmailTitle title
messagesSelector = chromeSelectorBase <&&> isMessagesTitle <$> title
slackSelector = className =? "Slack"
spotifySelector = className =? "Spotify"
transmissionSelector = fmap (isPrefixOf "Transmission") title
volumeSelector = className =? "Pavucontrol"

virtualClasses =
  [ (gmailSelector, "Gmail")
  , (messagesSelector, "Messages")
  , (chromeSelector, "Chrome")
  , (transmissionSelector, "Transmission")
  ]

-- Commands

chromeCommand = "google-chrome-stable"
elementCommand = "element-desktop"
emacsCommand = "emacsclient -c"
gmailCommand =
  "google-chrome-stable --new-window https://mail.google.com/mail/u/0/#inbox"
htopCommand = "alacritty --title htop -e htop"
messagesCommand =
  "google-chrome-stable --new-window https://messages.google.com/web/conversations"
slackCommand = "slack"
spotifyCommand = "spotify"
transmissionCommand = "transmission-gtk"
volumeCommand = "pavucontrol"

-- Startup hook

hostNameToAction =
  M.fromList [ ("ryzen-shine", return ())
             ]

myStartup = do
  setToggleActiveAll AVOIDSTRUTS True
  setToggleActiveAll GAPS True
  setToggleActiveAll NOBORDERS True
  hostName <- io getHostName
  M.findWithDefault (return ()) hostName hostNameToAction

-- Magnify

data DisableOnTabbedCondition = DisableOnTabbedCondition deriving (Read, Show)

instance ModifierCondition DisableOnTabbedCondition where
  shouldApply _ = do
    not . isInfixOf "Tabbed" . description . W.layout <$> currentWorkspace

disableOnTabbed = ConditionalLayoutModifier DisableOnTabbedCondition

myMagnify = ModifiedLayout $ disableOnTabbed (Mag 1 (1.3, 1.3) On (AllWins 1))

-- Toggles
unmodifyLayout (ModifiedLayout _ x') =  x'

selectLimit =
  myDmenu ["2", "3", "4"] >>= (setLimit . read)

data MyToggles
  = LIMIT
  | GAPS
  | MAGICFOCUS
  | MAGNIFY
  | AVOIDSTRUTS
  deriving (Read, Show, Eq, Typeable)

instance Transformer MyToggles Window where
  transform LIMIT x k = k (limitSlice 2 x) unmodifyLayout
  transform GAPS x k = k (smartSpacing 5 x) unmodifyLayout
  transform MAGICFOCUS x k = k (magicFocus x) unmodifyLayout
  transform MAGNIFY x k = k (myMagnify x) unmodifyLayout
  transform AVOIDSTRUTS x k = k (avoidStruts x) unmodifyLayout

myToggles = [LIMIT, GAPS, MAGICFOCUS, MAGNIFY, AVOIDSTRUTS]
otherToggles = [NBFULL, MIRROR, NOBORDERS, SMARTBORDERS]
toggleHandlers =
  [ (Toggle GAPS, toggleAll)
  , (Toggle MAGNIFY, toggleAll)
  , (Toggle AVOIDSTRUTS, toggleAll)
  ]

instance Eq (Toggle Window) where
  (Toggle v) == v2 = Just v == fromToggle v2

fromToggle :: forall t. Typeable t => Toggle Window -> Maybe t
fromToggle (Toggle v) =
  if typeOf v == typeRep (Proxy :: Proxy t) then
    Just $ unsafeCoerce v
  else
    Nothing

currentWorkspace = W.workspace . W.current <$> gets windowset
isToggleActiveInCurrent t = currentWorkspace >>= isToggleActive t

followIfNoMagicFocus =
  followOnlyIf $ maybe False not <$> isToggleActiveInCurrent MAGICFOCUS

togglesMap =
  fmap M.fromList $ sequence $
  map toggleTuple myToggles ++ map toggleTuple otherToggles
  where
    toggleTuple toggle = do
      toggleString <- toggleToStringWithState toggle
      return (toggleString, Toggle toggle)

toggleStateToString = maybe "N/A" (ifL "ON" "OFF")

toggleToStringWithState :: (Transformer t Window, Show t) => t -> X String
toggleToStringWithState toggle =
  printf "%s (%s)" (show toggle) . toggleStateToString <$>
  isToggleActiveInCurrent toggle

selectToggle =
  togglesMap >>= DM.menuMapArgs "rofi" myDmenuArgs >>= flip whenJust runToggle

runToggle toggle =
  let f = fromMaybe sendMessage $ lookup toggle toggleHandlers
      in f toggle

toggleAll (Toggle toggle) = void $ runMaybeT $ do
  active <- MaybeT $ isToggleActiveInCurrent toggle
  lift $ setToggleActiveAll toggle (not active)

mapWorkspaces f = withWindowSet $ \ws -> do
   let c = W.workspace . W.current $ ws
       v = map W.workspace . W.visible $ ws
       h = W.hidden ws
   mapM f (c : v ++ h)

toggleInState t s ws  = fmap (/= s) (isToggleActive t ws)

setToggleActive toggle active ws =
  toggleInState toggle (Just active) ws >>=/
  flip when (sendMessageWithNoRefresh (Toggle toggle) ws >> windows id)

-- Ambiguous type reference without signature
setToggleActiveCurrent :: (Transformer t Window) => t -> Bool -> X ()
setToggleActiveCurrent t a = void $ currentWorkspace >>= setToggleActive t a

setToggleActiveAll :: (Transformer t Window) => t -> Bool -> X ()
setToggleActiveAll t a = void $ mapWorkspaces (setToggleActive t a)

deactivateFull = setToggleActiveCurrent NBFULL False

toggleOr toggle toState action =
  (currentWorkspace >>= setToggleActive toggle toState) >>= ((`when` action) . not)

deactivateFullOr = toggleOr NBFULL False
deactivateFullAnd action = sequence_ [deactivateFull, action]

andDeactivateFull action = sequence_ [action, deactivateFull]

goFullscreen = sendMessage $ JumpToLayout "Tabbed"

-- Layout setup

myTabConfig =
  def { activeBorderColor = "#66cccc" }

rename newName = RN.renamed [RN.Replace newName]

layoutsStart layout = (layout, [Layout layout])

(|||!) (joined, layouts) newLayout =
    (joined ||| newLayout, layouts ++ [Layout newLayout])

layoutInfo =
  layoutsStart (rename "4 Columns" (multiCol [1, 1, 1] 2 0.0 (-0.5))) |||!
  rename "3 Columns" (multiCol [1, 1] 2 0.01 (-0.5)) |||!
  rename "Grid" Grid |||!
  rename "Large Main" (Tall 1 (3 / 100) (3 / 4)) |||!
  rename "2 Columns" (Tall 1 (3 / 100) (1 / 2)) |||!
  Accordion |||! simpleCross |||! myTabbed
    where
      myTabbed = rename "Tabbed" $ tabbed shrinkText (theme robertTheme)

layoutList = snd layoutInfo

layoutNames = [description layout | layout <- layoutList]

selectLayout = myDmenu layoutNames >>= (sendMessage . JumpToLayout)

myLayoutHook =
  minimizeNoDescription .
  boringAuto .
  mkToggle1 AVOIDSTRUTS .
  mkToggle1 MIRROR .
  mkToggle1 LIMIT .
  mkToggle1 GAPS .
  mkToggle1 MAGICFOCUS .
  mkToggle1 NBFULL .
  mkToggle1 MAGNIFY .
  mkToggle1 NOBORDERS .
  mkToggle1 SMARTBORDERS .
  lessBorders Screen $ fst layoutInfo

-- WindowBringer

myWindowBringerConfig =
  def { menuCommand = "rofi"
      , menuArgs = myDmenuArgs ++ ["-format", "i"]
      , windowTitler = myDecorateName
      }

classIfMatches window entry =
  if' <$> runQuery (fst entry) window <*>
      pure (Just $ snd entry) <*>
      pure Nothing

getClassRaw w = fmap resClass $ withDisplay $ io . flip getClassHint w

getVirtualClass = flip findM virtualClasses . classIfMatches

getClass w = fromMaybe <$> getClassRaw w <*> getVirtualClass w

{-# NOINLINE desktopEntriesMap #-}
desktopEntriesMap :: MM.MultiMap String DesktopEntry
desktopEntriesMap =
  unsafePerformIO $
        indexDesktopEntriesByClassName <$> getDirectoryEntriesDefault

lookupIconFromClasses classes =
  getFirst $ fold $ First . deIcon <$>
             (classes >>= idAndLower >>= flip MM.lookup desktopEntriesMap)
    where idAndLower value = [value, map toLower value]

xGetWindowProperty8 :: Atom -> Window -> X (Maybe [CChar])
xGetWindowProperty8 a w = withDisplay $ \dpy -> io $ getWindowProperty8 dpy a w

getEWMHClasses w = do
  atom <- withDisplay $ \d -> io $ internAtom d "WM_CLASS" False
  mValue <- fmap (UTF8.decode . map fromIntegral) <$> xGetWindowProperty8 atom w
  pure $ filter (not . null) $ splitOn "\NUL" $ join $ maybeToList mValue

myDecorateName :: WindowSpace -> Window -> X String
myDecorateName ws w = do
  name <- show <$> getName w
  classes <- getEWMHClasses w
  classTitle <- getClass w
  workspaceToName <- getWorkspaceNames'
  let iconName = fromMaybe (map toLower $ head classes) $
                 lookupIconFromClasses classes
      entryString = printf "%-20s%-40s %+30s in %s \0icon\x1f%s"
                    classTitle (take 40 name) " "
                    (fromMaybe "" $ workspaceToName (W.tag ws)) iconName
  return entryString

menuIndexArgs :: MonadIO m => String -> [String] -> [(String, a)] ->
               m (Maybe a)
menuIndexArgs menuCmd args selectionPairs = do
  selection <- menuFunction (map fst selectionPairs)
  pure $ snd <$> (readMay selection >>= atMay selectionPairs)
      where
        menuFunction = DM.menuArgs menuCmd args

-- This needs access to X in order to unminimize, which means that it can't be
-- done with the existing window bringer interface
myWindowAct c@WindowBringerConfig {menuCommand = cmd, menuArgs = args}
            filterVisible action = do
  visible <- visibleWindows
  currentlyFullscreen <- isToggleActiveInCurrent NBFULL
  let actualConfig
        | fromMaybe False currentlyFullscreen = c
        | filterVisible = c {windowFilter = not . flip elem visible}
        | otherwise = c
  ws <- M.toList <$> windowMap' actualConfig
  selection <- menuIndexArgs cmd args ws
  whenJust selection action

doBringWindow window =
  maximizeWindow window >> windows (W.focusWindow window . bringWindow window)

myWindowAction filterVisible =
  andDeactivateFull . maybeUnminimizeAfter . myWindowAct myWindowBringerConfig filterVisible

myGoToWindow =
  myWindowAction False $ windows . greedyFocusWindow

myBringWindow = myWindowAction True doBringWindow

myReplaceWindow =
  swapMinimizeStateAfter $
  myWindowAct myWindowBringerConfig True $ windows . swapFocusedWith

-- Workspace Names for EWMH

setWorkspaceNames :: X ()
setWorkspaceNames = withWindowSet $ \s -> withDisplay $ \dpy -> do
  sort' <- getSortByIndex
  let ws = sort' $ W.workspaces s
      tagNames = map W.tag ws
      getName tag = maybe "" (" " ++) <$> getWorkspaceName tag
      getFullName :: String -> X String
      getFullName tag = printf "%s%s" tag <$> getName tag
  names <- mapM getFullName tagNames
  r <- asks theRoot
  a <- getAtom "_NET_DESKTOP_FULL_NAMES"
  c <- getAtom "UTF8_STRING"
  let names' = map fromIntegral $ concatMap ((++[0]) . UTF8String.encode) names
  io $ changeProperty8 dpy r a c propModeReplace names'


-- Toggleable fade

newtype ToggleFade a =
  ToggleFade { fadesMap :: M.Map a Bool }
  deriving (Typeable, Read, Show)

instance (Typeable a, Read a, Show a, Ord a) =>
         ExtensionClass (ToggleFade a) where
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
getWorkspaceToScreen =
  M.fromList . mapP' (W.tag . W.workspace) W.screen <$> getScreens
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
  XS.get >>= XS.put . (ToggleFade . f . fadesMap)

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

-- XXX: The idea behind this was that the normal fullscreen can be annoying if a
-- new window opens, but this behavior is even more annoying than that, so
-- nevermind
goFullscreenDWIM =
  withWorkspace $ \ws -> do
    wins <- windowsWithFocusedClass ws
    if length wins > 1
      then goFullscreen
      else minimizeOtherClassesInWorkspace

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
  filterM (fmap predicate . getClass) (W.integrate workspace)

getMatchingUnmatching =
  partition <$> ((. snd) <$> getClassMatchesCurrent) <*> getWindowClassPairs

getWindowClassPairs = mapM windowToClassPair =<< workspaceWindows

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

restoreOrMinimizeOtherClasses = maximizedOtherClass >>=
  ifL restoreAllMinimized minimizeOtherClassesInWorkspace . null

restoreThisClassOrMinimizeOtherClasses = minimizedSameClass >>= \ws ->
  if' (null ws) minimizeOtherClassesInWorkspace $ restoreAll ws

getClassPair w = (, w) <$> getClass w

windowClassPairs = withWindowSet $ mapM getClassPair . W.allWindows
classToWindowMap = MM.fromList <$> windowClassPairs
allClasses = sort . MM.keys <$> classToWindowMap
thisClass = withWindowSet $ sequence . (getClass <$.> W.peek)

nextClass = do
  classes <- allClasses
  current <- thisClass
  let index = join $ elemIndex <$> current <$$> classes
  return $ fmap (\i -> cycle classes !! (i + 1)) index

classWindow c = do
  m <- classToWindowMap
  return (listToMaybe . flip MM.lookup m =<< c)

nextClassWindow = nextClass >>= classWindow

focusNextClass' =
  (windows . maybe id greedyFocusWindow) =<< nextClassWindow
focusNextClass = sameClassOnly focusNextClass'

selectClass = myDmenu =<< allClasses

-- Gather windows of same class

allWindows = concat <$> mapWorkspaces (return . W.integrate' . W.stack)

windowsMatchingClass klass =
  allWindows >>= filterM (((== klass) <$>) . getClass)

gatherClass klass = restoreFocus $
  windowsMatchingClass klass >>= mapM_ doBringWindow

gatherThisClass = thisClass >>= flip whenJust gatherClass

-- Window switching

-- Use greedyView to switch to the correct workspace, and then focus on the
-- appropriate window within that workspace.
greedyFocusWindow w ws =
  W.focusWindow w $
  W.greedyView (fromMaybe (W.currentTag ws) $ W.findTag w ws) ws

shiftThenView i = W.greedyView i . W.shift i

greedyBringWindow w = greedyFocusWindow w . bringWindow w

shiftToEmptyAndView =
  doTo Next emptyWS DWO.getSortByOrder (windows . shiftThenView)

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
  maybe s (flip (shiftThenViewOtherScreen ws) s) (W.peek s)

shiftToEmptyNextScreen =
  doTo Next emptyWS DWO.getSortByOrder $ windows . shiftCurrentToWSOnOtherScreen

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

nearFullFloat = customFloating $ W.RationalRect l t w h
  where
    h = 0.9
    w = 0.9
    t = 0.95 -h
    l = 0.95 -w


scratchpads =
  [ NS "element" elementCommand elementSelector nearFullFloat
  , NS "gmail" gmailCommand gmailSelector nearFullFloat
  , NS "htop" htopCommand (title =? "htop") nearFullFloat
  , NS "messages" messagesCommand messagesSelector nearFullFloat
  , NS "slack" slackCommand slackSelector nearFullFloat
  , NS "spotify" spotifyCommand spotifySelector nearFullFloat
  , NS "transmission" transmissionCommand transmissionSelector nearFullFloat
  , NS "volume" volumeCommand volumeSelector nearFullFloat
  ]


myScratchPadManageHook = namedScratchpadManageHook scratchpads
-- We need this event hook because some scratchpad applications (Spotify) don't
-- actually properly set their class at startup.
myScratchPadEventHook
  =  dynamicPropertyChange "WM_CLASS" myScratchPadManageHook
  <> dynamicPropertyChange "WM_NAME" myScratchPadManageHook

runScratchPadManageHookOnCurrent =
  join (withFocusedD (Endo id) $ runQuery myScratchPadManageHook) >>= windows . appEndo

scratchPadIsDisplayed name = join $ withFocusedD False query
  where
    query = maybe (const $ return False) (runQuery . NS.query) scratchpadInfo
    scratchpadInfo = find ((name ==) . NS.name) scratchpads

manageIfScratchPadIsDisplayed name =
  scratchPadIsDisplayed name >>= (`when` runScratchPadManageHookOnCurrent)

-- TODO: This doesnt work well with minimized windows
doScratchpad name = do
  maybeUnminimizeAfter $ deactivateFullAnd $ namedScratchpadAction scratchpads name
  manageIfScratchPadIsDisplayed name

-- Raise or spawn

myRaiseNextMaybe =
  ((deactivateFullAnd . maybeUnminimizeAfter) .) .
  raiseNextMaybeCustomFocus greedyFocusWindow

myBringNextMaybe =
  ((deactivateFullAnd . maybeUnminimizeAfter) .) .
  raiseNextMaybeCustomFocus greedyBringWindow

bindBringAndRaise :: KeyMask
                  -> KeySym
                  -> X ()
                  -> Query Bool
                  -> [((KeyMask, KeySym), X ())]
bindBringAndRaise mask sym start query =
    [ ((mask, sym), doRaiseNext)
    , ((mask .|. controlMask, sym), myBringNextMaybe start query)
    , ((mask .|. shiftMask, sym), doRaiseNext)
    ]
  where doRaiseNext = myRaiseNextMaybe start query

bindBringAndRaiseMany :: [(KeyMask, KeySym, X (), Query Bool)]
                      -> [((KeyMask, KeySym), X ())]
bindBringAndRaiseMany = concatMap (\(a, b, c, d) -> bindBringAndRaise a b c d)

-- Screen shift

shiftToNextScreen ws =
  case W.visible ws of
    W.Screen i _ _:_ -> W.view (W.tag i) $ W.shift (W.tag i) ws
    _ -> ws

shiftToNextScreenX = windows shiftToNextScreen

getNextScreen ws =
  minimumBy compareScreen candidates
    where currentId = W.screen $ W.current ws
          otherScreens = W.visible ws
          largerId = filter ((> currentId) . W.screen) otherScreens
          compareScreen a b = compare (W.screen a) (W.screen b)
          candidates =
            case largerId of
              [] -> W.current ws:otherScreens -- Ensure a value will be selected
              _ -> largerId

goToNextScreen ws =
  if screenEq nScreen currScreen then ws
     else ws { W.current = nScreen
             , W.visible = currScreen : trimmedVisible
             }
  where
    currScreen = W.current ws
    nScreen = getNextScreen ws
    screenEq a b = W.screen a == W.screen b
    trimmedVisible =
      filter (not . screenEq nScreen) $ W.visible ws

goToNextScreenX = windows goToNextScreen

-- Key bindings

volumeUp = spawn "set_volume --unmute --change-volume +5"
volumeDown = spawn "set_volume --unmute --change-volume -5"
mute = spawn "set_volume --toggle-mute"

shiftToEmptyOnScreen direction =
  followingWindow (windowToScreen direction True) >> shiftToEmptyAndView

directionalUp = xK_w
directionalDown = xK_s
directionalLeft = xK_a
directionalRight = xK_d

buildDirectionalBindings mask commandFn =
  [ ((mask, directionalUp   ), commandFn U)
  , ((mask, directionalDown ), commandFn D)
  , ((mask, directionalLeft ), commandFn L)
  , ((mask, directionalRight), commandFn R)
  ]

myWindowGo direction = do
  layoutName <- description . W.layout <$> currentWorkspace
  if "Tabbed" `isInfixOf` layoutName
  then
    case direction of
      D -> windows W.focusUp
      L -> windows W.focusUp
      R -> windows W.focusDown
      U -> windows W.focusDown
  else windowGo direction True

addKeys conf@XConfig { modMask = modm } =

    -- Directional navigation

    buildDirectionalBindings modm myWindowGo ++
    buildDirectionalBindings
     (modm .|. shiftMask) (`windowSwap` True) ++
    buildDirectionalBindings
     (modm .|. controlMask)  (followingWindow . (`windowToScreen` True)) ++
    buildDirectionalBindings hyper (`screenGo` True) ++
    buildDirectionalBindings
     (hyper .|. shiftMask) (followingWindow . (`screenSwap` True)) ++
    buildDirectionalBindings
     (hyper .|. controlMask) shiftToEmptyOnScreen ++

    -- Specific program spawning
    bindBringAndRaiseMany
    [ (modalt, xK_c, spawn chromeCommand, chromeSelector)
    ] ++

    -- ScratchPads
    [ ((modalt, xK_e), doScratchpad "element")
    , ((modalt, xK_g), doScratchpad "gmail")
    , ((modalt, xK_h), doScratchpad "htop")
    , ((modalt, xK_m), doScratchpad "messages")
    , ((modalt, xK_k), doScratchpad "slack")
    , ((modalt, xK_s), doScratchpad "spotify")
    , ((modalt, xK_t), doScratchpad "transmission")
    , ((modalt, xK_v), doScratchpad "volume")

    -- Specific program spawning

    , ((modm, xK_p), spawn "rofi -show drun -show-icons")
    , ((modm .|. shiftMask, xK_p), spawn "rofi -show run")

    -- Window manipulation

    , ((modm, xK_g), myGoToWindow)
    , ((modm, xK_b), myBringWindow)
    , ((modm .|. shiftMask, xK_b), myReplaceWindow)
    , ((modm .|. controlMask, xK_space), deactivateFullOr goFullscreen)
    , ((modm, xK_m), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_m),
       deactivateFullOr $ withLastMinimized maximizeWindowAndFocus)
    , ((modm, xK_x), addHiddenWorkspace "NSP" >> windows (W.shift "NSP"))
    , ((modalt, xK_space), deactivateFullOr restoreOrMinimizeOtherClasses)
    , ((modalt, xK_Return), deactivateFullAnd restoreAllMinimized)
    , ((hyper, xK_g), gatherThisClass)


    -- Focus/Layout manipulation

    , ((modm, xK_e), goToNextScreenX)
    , ((modm, xK_slash), sendMessage $ Toggle MIRROR)
    , ((modm, xK_backslash),
       cycleWorkspaceOnCurrentScreen [xK_Super_L] xK_backslash xK_slash)
    , ((modm, xK_space), deactivateFullOr $ sendMessage NextLayout)
    , ((modm, xK_z), shiftToNextScreenX)
    , ((modm .|. shiftMask, xK_z), shiftToEmptyNextScreen)
    , ((modm .|. shiftMask, xK_h), shiftToEmptyAndView)
    , ((hyper, xK_5), getWorkspaceDmenu >>= windows . SW.swapWithCurrent)

    -- These need to be rebound to support boringWindows
    , ((modm, xK_m), focusMaster)
    , ((modm, xK_Tab), focusNextClass)
    , ((hyper, xK_e), moveTo Next emptyWS)


    -- Miscellaneous XMonad

    , ((hyper, xK_1), toggleFadingForActiveWindow)
    , ((hyper .|. shiftMask, xK_1), toggleFadingForActiveWorkspace)
    , ((hyper .|. controlMask, xK_1), toggleFadingForActiveScreen)
    , ((hyper, xK_t), selectToggle)
    , ((modalt, xK_4), selectLimit)
    , ((hyper, xK_3), addWorkspacePrompt def)
    , ((modalt, xK_3), selectWorkspace def)
    , ((hyper .|. mod1Mask, xK_3), removeWorkspace)
    , ((hyper .|. mod1Mask, xK_r), renameWorkspace def)

    -- Non-XMonad

    , ((modm, xK_v), spawn "xclip -o | xdotool type --file -")
    , ((hyper, xK_v), spawn "rofi_clipit.sh")
    , ((hyper, xK_p), spawn "rofi-pass")
    , ((hyper, xK_h), spawn "screenshot.sh")
    , ((hyper, xK_c), spawn "shell_command.sh")
    , ((hyper, xK_x), spawn "rofi_command.sh")
    , ((hyper .|. shiftMask, xK_l), spawn "dm-tool lock")
    , ((hyper, xK_l), selectLayout)
    , ((hyper, xK_k), spawn "rofi_kill_process.sh")
    , ((hyper .|. shiftMask, xK_k), spawn "rofi_kill_all.sh")
    , ((hyper, xK_r), spawn "rofi-systemd")
    , ((hyper, xK_9), spawn "start_synergy.sh")
    , ((hyper, xK_slash), spawn "toggle_taffybar")
    , ((hyper, xK_space), spawn "skippy-xd")
    , ((hyper, xK_i), spawn "rofi_select_input.hs")
    , ((hyper, xK_o), spawn "rofi_paswitch")
    , ((modm, xK_apostrophe), spawn "load_default_map")
    , ((modalt, xK_apostrophe), spawn "load_xkb_map")

    -- Media keys

    -- playerctl
    , ((modm, xK_semicolon), spawn "playerctl play-pause")
    , ((0, xF86XK_AudioPause), spawn "playerctl play-pause")
    , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((modm, xK_l), spawn "playerctl next")
    , ((0, xF86XK_AudioNext), spawn "playerctl next")
    , ((modm, xK_j), spawn "playerctl previous")
    , ((0, xF86XK_AudioPrev), spawn "playerctl previous")

    -- Volume control
    , ((0, xF86XK_AudioRaiseVolume), volumeUp)
    , ((0, xF86XK_AudioLowerVolume), volumeDown)
    , ((0, xF86XK_AudioMute), mute)
    , ((modm, xK_i), volumeUp)
    , ((modm, xK_k), volumeDown)
    , ((modm, xK_u), mute)
    , ((hyper .|. shiftMask, xK_q), spawn "toggle_mute_current_window.sh")
    , ((hctrl, xK_q), spawn "toggle_mute_current_window.sh only")

    , ((0, xF86XK_MonBrightnessUp), spawn "brightness.sh 5")
    , ((0, xF86XK_MonBrightnessDown), spawn "brightness.sh -5")

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
