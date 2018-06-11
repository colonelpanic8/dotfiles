{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances,
             MultiParamTypeClasses, ExistentialQuantification,
             FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module Main where

import qualified Codec.Binary.UTF8.String as UTF8String (encode)
import qualified Control.Arrow as A
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.List
import           Data.List.Split
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.MultiMap as MM
import           Data.Proxy
import           Data.Tuple.Sequence (sequenceT)
import           Data.Typeable
import           Graphics.X11.ExtraTypes.XF86
import           Network.HostName
import           System.Directory
import           System.FilePath.Posix
import           System.Process
import           PagerHints
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
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.WorkspaceHistory
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
import           XMonad.Util.NamedWindows (getName)
import           XMonad.Util.Run
import           XMonad.Util.WorkspaceCompare

myConfig = def
  { modMask = mod4Mask
  , terminal = "termite"
  , manageHook = myManageHook <+> manageHook def
  , layoutHook = myLayoutHook
  , borderWidth = 0
  , normalBorderColor = "#000000"
  , focusedBorderColor = "#ffff00"
  , logHook =
      updatePointer (0.5, 0.5) (0, 0) +++
      toggleFadeInactiveLogHook 0.9 +++ workspaceHistoryHook +++
      setWorkspaceNames
  , handleEventHook =
      fullscreenEventHook +++ followIfNoMagicFocus +++ minimizeEventHook
  , startupHook = myStartup
  , keys = customKeys (const []) addKeys
  }
  where
    x +++ y = mappend y x

myNavigation2DConfig = def { defaultTiledNavigation = centerNavigation }

main =
  xmonad .
  docks .
  pagerHints .
  ewmh .
  withNavigation2DConfig myNavigation2DConfig $
  myConfig

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

myDmenuArgs = ["-dmenu", "-i"]

myDmenu = DM.menuArgs "rofi" myDmenuArgs

getWorkspaceDmenu = myDmenu (workspaces myConfig)

-- Selectors

isHangoutsTitle = isPrefixOf "Google Hangouts"
isGmailTitle t = isInfixOf "@gmail.com" t && isInfixOf "Gmail" t
isChromeClass = isInfixOf "chrome"
chromeSelectorBase = isChromeClass <$> className

chromeSelector =
  chromeSelectorBase <&&>
  (\t -> not $ any ($ t) [isHangoutsTitle, isGmailTitle]) <$> title
spotifySelector = className =? "Spotify"
emacsSelector = className =? "Emacs"
transmissionSelector = fmap (isPrefixOf "Transmission") title
hangoutsSelector = chromeSelectorBase <&&> fmap isHangoutsTitle title
gmailSelector = chromeSelectorBase <&&> fmap isGmailTitle title
volumeSelector = className =? "Pavucontrol"

virtualClasses =
  [ (hangoutsSelector, "Hangouts")
  , (gmailSelector, "Gmail")
  , (chromeSelector, "Chrome")
  , (transmissionSelector, "Transmission")
  ]

-- Commands

hangoutsCommand = "start_hangouts.sh"
gmailCommand = "start_chrome.sh --new-window https://mail.google.com/mail/u/0/#inbox"
spotifyCommand = "spotify"
chromeCommand = "start_chrome.sh"
emacsCommand = "emacsclient -c"
htopCommand = "termite -e htop -t htop"
transmissionCommand = "transmission-gtk"
volumeCommand = "pavucontrol"
taffybarCommand = "restart_taffybar.sh"

-- Startup hook

tvScreenId :: ScreenId
tvScreenId = 1

disableTVFading = setFading (Just tvScreenId) False

hostNameToAction =
  M.fromList [ ("imalison-arch", disableTVFading >> setToggleActiveAll GAPS True)
             , ("imalison-uber-loaner", return ())
             ]

myStartup = do
  spawn "systemctl --user start wm.target"
  hostName <- io getHostName
  M.findWithDefault (return ()) hostName hostNameToAction

-- Manage hook

myManageHook = maybeReplaceTargetHook <+>
  composeOne
  [ isFullscreen -?> doFullFloat ]

-- Toggles

unmodifyLayout (ModifiedLayout _ x') =  x'

selectLimit =
  myDmenu ["2", "3", "4"] >>= (setLimit . read)

data MyToggles
  = LIMIT
  | GAPS
  | MAGICFOCUS
  deriving (Read, Show, Eq, Typeable)

instance Transformer MyToggles Window where
  transform LIMIT x k = k (limitSlice 2 x) unmodifyLayout
  transform GAPS x k = k (smartSpacing 10 x) unmodifyLayout
  transform MAGICFOCUS x k = k (magicFocus x) unmodifyLayout

myToggles = [LIMIT, GAPS, MAGICFOCUS]
otherToggles = [NBFULL, MIRROR]
toggleHandlers = [(Toggle GAPS, toggleAll)]

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

selectLayout = myDmenu layoutNames >>= (sendMessage . JumpToLayout)

myLayoutHook =
  avoidStruts .
  minimize .
  boringAuto .
  mkToggle1 MIRROR .
  mkToggle1 LIMIT .
  mkToggle1 GAPS .
  mkToggle1 MAGICFOCUS .
  mkToggle1 NBFULL .
  lessBorders Screen $ fst layoutInfo

-- WindowBringer

myWindowBringerConfig =
  def { menuCommand = "rofi"
      , menuArgs = myDmenuArgs
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

data ChromeInfo = ChromeInfo { tabId :: Int
                             , tabUri :: String
                             , tabTitle :: String
                             } deriving (Eq, Show)

getChromeTabInfo = do
  output <- runProcessWithInput "chromix-too" ["ls"] ""
  return $ M.fromList $ map parseChromixLine $ lines output
    where parseChromixLine line =
            case splitOn " " line of
              [] -> undefined
              tid:uri:rest -> let ttl = concat rest in
                              (printf "%s - %s" tid ttl :: String,
                               ChromeInfo { tabId = read tid
                                          , tabUri = uri
                                          , tabTitle = ttl
                                          })
              [_] -> undefined

selectChromeTab WindowBringerConfig { menuCommand = cmd
                                    , menuArgs = args
                                    } =
  liftIO getChromeTabInfo >>= void . DM.menuMapArgs cmd args

chromeTabAction doSplit action selected =
  case selected of
    Left wid -> action wid
    Right ChromeInfo { tabId = tid } ->
      liftIO $ do
        let command = if doSplit then
                "split_tab_by_id.sh %s"
              else
                "focus_tab_by_id.sh %s"
        spawn $ printf command $ show tid
        return ()

-- This needs access to X in order to unminimize, which means that it can't be
-- done with the existing window bringer interface
myWindowAct c@WindowBringerConfig {menuCommand = cmd, menuArgs = args} action = do
  visible <- visibleWindows
  -- Uncomment filter to remove windows that are visible
  ws <- windowMap' c -- {windowFilter = not . flip elem visible}
    -- chromeTabs <- liftIO getChromeTabInfo
  let options = M.union (M.map Left ws) (M.map Right M.empty)
  selection <- DM.menuMapArgs cmd args options
  whenJust selection action

doBringWindow window =
  maximizeWindow window >> windows (W.focusWindow window . bringWindow window)

myWindowAction = andDeactivateFull . maybeUnminimizeAfter .
                 myWindowAct myWindowBringerConfig

myGoToWindow =
  myWindowAction $ chromeTabAction False $ windows . greedyFocusWindow

myBringWindow = myWindowAction $ chromeTabAction True doBringWindow

myReplaceWindow =
  swapMinimizeStateAfter $
  myWindowAct myWindowBringerConfig $
  chromeTabAction True (windows . swapFocusedWith)

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
  let index = join $ elemIndex <$> current <$$> classes
  return $ fmap (\i -> cycle classes !! (i + 1)) index

classWindow c = do
  m <- classToWindowMap
  return $ join $ listToMaybe <$> (flip MM.lookup m <$> c)

nextClassWindow = nextClass >>= classWindow

focusNextClass' =
  join $ windows . maybe id greedyFocusWindow <$> nextClassWindow
focusNextClass = sameClassOnly focusNextClass'

selectClass = join $ myDmenu <$> allClasses

-- Chrome auto minimization

data ReplaceOnNew
  = NoTarget
  | DontTarget
  | Target Window
  deriving (Typeable, Read, Show)

instance ExtensionClass ReplaceOnNew where
  initialValue = NoTarget
  extensionType = PersistentExtension

mapWindows f = W.mapWorkspace workspaceHelper
  where
    stackHelper stack = W.Stack
          { W.focus = f $ W.focus stack
          , W.up = map f $ W.up stack
          , W.down = map f $ W.down stack
          }
    workspaceHelper ws@W.Workspace {W.stack = stack} =
      ws { W.stack = stackHelper <$> stack }

swapWindows a b =
  mapWindows helper
  where helper w
          | w == a = b
          | w == b = a
          | otherwise = w

getTarget = do
  t <- XS.get
  case t of
    Target w -> return $ Just w
    DontTarget -> return Nothing
    NoTarget -> return Nothing

maybeReplaceTarget :: Window -> X ()
maybeReplaceTarget window = do
  t <- getTarget
      -- We have an insertUp here to ensure the target isn't deleted
  let modifyStackSet target = W.insertUp target . swapWindows window target
      replaceTarget target =
        windows (modifyStackSet target) >> minimizeWindow target >>
        XS.put (initialValue :: ReplaceOnNew)
  whenJust t replaceTarget

maybeReplaceTargetHook = ask >>= (liftX . maybeReplaceTarget) >> return (Endo id)

setReplaceTarget = withFocused $ XS.put . Target

getWindowWS a = withWindowSet $ \ws -> return $ listToMaybe
    [ w | w <- W.workspaces ws, has a (W.stack w) ]
    where has _ Nothing = False
          has _ (Just _) = True

replaceWindow original replacement =
  W.delete original . swapWindows original replacement

chromeReplaceKill =
  withFocused $ \w -> do
    vClass <- getClass w
    if vClass == "Chrome" then
      do
        replacement <-
          runMaybeT $ do
            ws <- MaybeT $ join . fmap W.stack <$> getWindowWS w
            MaybeT $
              listToMaybe <$>
              (intersect <$> minimizedWindows <*> windowsWithSameClass w ws)
        let doReplace rep = do
              maximizeWindow rep
              windows $ replaceWindow w rep
        maybe kill doReplace replacement
    else
      kill

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

volumeUp = spawn "set_volume.sh --unmute --change-volume +5"
volumeDown = spawn "set_volume.sh --unmute --change-volume -5"
mute = spawn "set_volume.sh --toggle-mute"

shiftToEmptyOnScreen direction =
  followingWindow (windowToScreen direction True) >> shiftToEmptyAndView

addKeys conf@XConfig { modMask = modm } =

    -- Specific program spawning
    bindBringAndRaiseMany
    [ (modalt, xK_c, spawn chromeCommand, chromeSelector)
    , (modalt, xK_e, spawn emacsCommand, emacsSelector)
    , (modalt, xK_g, spawn gmailCommand, gmailSelector)
    , (modalt, xK_t, spawn transmissionCommand, transmissionSelector)
    ] ++

    -- ScratchPads
    [ ((modalt, xK_m), doScratchpad "htop")
    , ((modalt, xK_v), doScratchpad "volume")
    , ((modalt, xK_h), doScratchpad "hangouts")
    , ((modalt, xK_s), doScratchpad "spotify")
    , ((modalt .|. controlMask, xK_h),
       myRaiseNextMaybe (spawn hangoutsCommand) hangoutsSelector)
    , ((modalt .|. controlMask, xK_s),
       myRaiseNextMaybe (spawn spotifyCommand) spotifySelector)

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
    , ((modm .|. controlMask, xK_t),
       setReplaceTarget >> spawn "chromix-too open chrome://newtab")
    , ((modm .|. controlMask, xK_c), chromeReplaceKill)
    , ((hyper, xK_g), gatherThisClass)

    -- Directional navigation
    , ((modm, xK_w), windowGo U True)
    , ((modm, xK_s), windowGo D True)
    , ((modm, xK_a), windowGo L True)
    , ((modm, xK_d), windowGo R True)

    , ((modm .|. shiftMask, xK_w), windowSwap U True)
    , ((modm .|. shiftMask, xK_s), windowSwap D True)
    , ((modm .|. shiftMask, xK_a), windowSwap L True)
    , ((modm .|. shiftMask, xK_d), windowSwap R True)

    , ((modm .|. controlMask, xK_w), followingWindow $ windowToScreen U True)
    , ((modm .|. controlMask, xK_s), followingWindow $ windowToScreen D True)
    , ((modm .|. controlMask, xK_a), followingWindow $ windowToScreen L True)
    , ((modm .|. controlMask, xK_d), followingWindow $ windowToScreen R True)

    , ((hyper, xK_w), screenGo U True)
    , ((hyper, xK_s), screenGo D True)
    , ((hyper, xK_a), screenGo L True)
    , ((hyper, xK_d), screenGo R True)

    , ((hyper .|. shiftMask, xK_w), followingWindow $ screenSwap U True)
    , ((hyper .|. shiftMask, xK_s), followingWindow $ screenSwap D True)
    , ((hyper .|. shiftMask, xK_a), followingWindow $ screenSwap L True)
    , ((hyper .|. shiftMask, xK_d), followingWindow $ screenSwap R True)

    , ((hyper .|. controlMask, xK_w), shiftToEmptyOnScreen U)
    , ((hyper .|. controlMask, xK_s), shiftToEmptyOnScreen D)
    , ((hyper .|. controlMask, xK_a), shiftToEmptyOnScreen L)
    , ((hyper .|. controlMask, xK_d), shiftToEmptyOnScreen R)

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
    , ((hyper .|. mod1Mask, xK_r), renameWorkspace def)

    -- Non-XMonad

    , ((modm, xK_v), spawn "xclip -o | xdotool type --file -")
    , ((modm .|. controlMask, xK_s), spawn "split_current_chrome_tab.sh")
    , ((hyper, xK_v), spawn "rofi_clipit.sh")
    , ((hyper, xK_p), spawn "rofi-pass")
    , ((hyper, xK_h), spawn "screenshot.sh")
    , ((hyper, xK_c), spawn "shell_command.sh")
    , ((hyper, xK_x), spawn "rofi_command.sh")
    , ((hyper .|. shiftMask, xK_l), spawn "dm-tool lock")
    , ((hyper, xK_l), selectLayout)
    , ((hyper, xK_k), spawn "rofi_kill_process.sh")
    , ((hyper .|. shiftMask, xK_k),
       spawn "rofi_kill_all.sh")
    , ((hyper, xK_r), spawn "rofi_systemd.sh")
    , ((hyper, xK_0), spawn "tvpower.js")
    , ((modalt, xK_z), spawn "split_chrome_tab_to_next_screen.sh")
    , ((hyper, xK_9), spawn "start_synergy.sh")
    , ((hyper, xK_slash), spawn "toggle_taffybar.sh")
    , ((hyper, xK_space), spawn "skippy-xd")
    , ((hyper, xK_i), spawn "rofi_select_input.hs")
    , ((hyper, xK_o), spawn "rofi_paswitch.sh")

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
