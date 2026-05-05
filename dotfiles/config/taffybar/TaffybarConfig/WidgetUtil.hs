{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TaffybarConfig.WidgetUtil
  ( decorateWithClassAndBox,
    decorateWithClassAndBoxM,
    setFixedLabelWidth,
    setLabelAlignmentRecursively,
    stackInPill,
    usageLogoWidget,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.GI.Base (castTo)
import Data.Int (Int32)
import Data.Text (Text)
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Widget.Util
  ( buildContentsBox,
    pixbufNewFromFileAtScaleByHeight,
    widgetSetClassGI,
  )

-- | Wrap the widget in a "TaffyBox" (via 'buildContentsBox') and add a CSS class.
decorateWithClassAndBox :: (MonadIO m) => Text -> Gtk.Widget -> m Gtk.Widget
decorateWithClassAndBox klass widget = do
  boxed <- buildContentsBox widget
  widgetSetClassGI boxed klass

decorateWithClassAndBoxM :: (MonadIO m) => Text -> m Gtk.Widget -> m Gtk.Widget
decorateWithClassAndBoxM klass builder =
  builder >>= decorateWithClassAndBox klass

forEachLabelRecursively :: Gtk.Widget -> (Gtk.Label -> IO ()) -> IO ()
forEachLabelRecursively widget action = do
  maybeLabel <- castTo Gtk.Label widget
  for_ maybeLabel action

  maybeContainer <- castTo Gtk.Container widget
  case maybeContainer of
    Just container ->
      Gtk.containerGetChildren container >>= mapM_ (`forEachLabelRecursively` action)
    Nothing -> pure ()

setLabelAlignmentRecursively :: Float -> Gtk.Justification -> Gtk.Widget -> IO ()
setLabelAlignmentRecursively xalign justify widget =
  forEachLabelRecursively widget $ \label -> do
    Gtk.labelSetXalign label xalign
    Gtk.labelSetJustify label justify

setFixedLabelWidth :: Int32 -> Gtk.Label -> IO ()
setFixedLabelWidth width label = do
  Gtk.labelSetWidthChars label width
  Gtk.labelSetMaxWidthChars label width
  Gtk.labelSetEllipsize label Pango.EllipsizeModeEnd

stackInPill :: Text -> [TaffyIO Gtk.Widget] -> TaffyIO Gtk.Widget
stackInPill klass builders =
  decorateWithClassAndBoxM klass $ do
    widgets <- sequence builders
    liftIO $ do
      box <- Gtk.boxNew Gtk.OrientationVertical 0
      mapM_ (\w -> Gtk.boxPackStart box w False False 0) widgets
      Gtk.widgetShowAll box
      Gtk.toWidget box

usageLogoWidget :: FilePath -> Text -> IO Gtk.Widget
usageLogoWidget iconFile tooltip = do
  iconPath <- getUserConfigFile "taffybar" ("icons/" <> iconFile)
  iconWidget <-
    pixbufNewFromFileAtScaleByHeight 18 iconPath >>= \case
      Right pixbuf -> Gtk.toWidget =<< Gtk.imageNewFromPixbuf (Just pixbuf)
      Left _ -> Gtk.toWidget =<< Gtk.labelNew (Just "?")
  Gtk.widgetSetTooltipText iconWidget (Just tooltip)
  widgetSetClassGI iconWidget "usage-logo"
