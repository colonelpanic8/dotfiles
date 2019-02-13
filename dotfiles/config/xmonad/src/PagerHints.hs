module PagerHints (
  -- * Usage
  -- $usage
  pagerHints
) where

import Codec.Binary.UTF8.String (encode)
import Control.Monad
import Data.Monoid
import Foreign.C.Types (CInt)
import XMonad
import qualified XMonad.StackSet as W

-- $usage
--
-- You can use this module with the following in your @xmonad.hs@ file:
--
-- > import System.Taffybar.Hooks.PagerHints (pagerHints)
-- >
-- > main = xmonad $ ewmh $ pagerHints $ defaultConfig
-- > ...

-- | The \"Current Layout\" custom hint.
xLayoutProp :: X Atom
xLayoutProp = getAtom "_XMONAD_CURRENT_LAYOUT"

-- | The \"Visible Workspaces\" custom hint.
xVisibleProp :: X Atom
xVisibleProp = getAtom "_XMONAD_VISIBLE_WORKSPACES"

-- | Add support for the \"Current Layout\" and \"Visible Workspaces\" custom
-- hints to the given config.
pagerHints :: XConfig a -> XConfig a
pagerHints c =
  c { handleEventHook = handleEventHook c +++ pagerHintsEventHook
    , logHook = logHook c +++ pagerHintsLogHook
    }
  where x +++ y = x `mappend` y

-- | Update the current values of both custom hints.
pagerHintsLogHook :: X ()
pagerHintsLogHook = do
  withWindowSet
    (setCurrentLayout . description . W.layout . W.workspace . W.current)
  withWindowSet
    (setVisibleWorkspaces . map (W.tag . W.workspace) . W.visible)

-- | Set the value of the \"Current Layout\" custom hint to the one given.
setCurrentLayout :: String -> X ()
setCurrentLayout l = withDisplay $ \dpy -> do
  r <- asks theRoot
  a <- xLayoutProp
  c <- getAtom "UTF8_STRING"
  let l' = map fromIntegral (encode l)
  io $ changeProperty8 dpy r a c propModeReplace l'

-- | Set the value of the \"Visible Workspaces\" hint to the one given.
setVisibleWorkspaces :: [String] -> X ()
setVisibleWorkspaces vis = withDisplay $ \dpy -> do
  r  <- asks theRoot
  a  <- xVisibleProp
  c  <- getAtom "UTF8_STRING"
  let vis' = map fromIntegral $ concatMap ((++[0]) . encode) vis
  io $ changeProperty8 dpy r a c propModeReplace vis'

-- | Handle all \"Current Layout\" events received from pager widgets, and
-- set the current layout accordingly.
pagerHintsEventHook :: Event -> X All
pagerHintsEventHook ClientMessageEvent {
    ev_message_type = mt,
    ev_data = d
  } = withWindowSet $ \_ -> do
  a <- xLayoutProp
  when (mt == a) $ sendLayoutMessage d
  return (All True)
pagerHintsEventHook _ = return (All True)

-- | Request a change in the current layout by sending an internal message
-- to XMonad.
sendLayoutMessage :: [CInt] -> X ()
sendLayoutMessage evData = case evData of
  []   -> return ()
  x:_  -> if x < 0
            then sendMessage FirstLayout
            else sendMessage NextLayout
