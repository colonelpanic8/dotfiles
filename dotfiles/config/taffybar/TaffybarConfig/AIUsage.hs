{-# LANGUAGE OverloadedStrings #-}

-- | Configurable OpenAI and Anthropic usage sections.
--
-- The Hyprland config (scratchpads.lua) lets SUPER+ALT+C toggle whichever AI
-- app is currently selected via rofi_ai_scratchpad.sh, which records the
-- choice in $XDG_STATE_HOME/hypr/ai-scratchpad. By default, this widget reads
-- the same state file and shows the matching provider's usage section (OpenAI
-- for "codex", Anthropic for "claude"), switching live when the file changes.
-- Set TAFFYBAR_AI_USAGE_MODE=both to keep both provider sections visible.
module TaffybarConfig.AIUsage
  ( aiUsageWidget,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (lookupEnv)
import qualified System.FSNotify as FSNotify
import System.FilePath (takeFileName, (</>))
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.AnthropicUsage
  ( AnthropicUsageDisplayMode (AnthropicUsageDisplayRemaining),
    AnthropicUsageStackConfig (..),
    anthropicUsageSectionNewWith,
    defaultAnthropicUsageStackConfig,
  )
import System.Taffybar.Widget.OpenAIUsage
  ( OpenAIUsageDisplayMode (OpenAIUsageDisplayRemaining),
    OpenAIUsageStackConfig (..),
    defaultOpenAIUsageStackConfig,
    openAIUsageSectionNewWith,
  )
import System.Taffybar.Widget.Util
  ( UsageWindowLabelParts (..),
    UsageWindowPosition (..),
  )
import TaffybarConfig.WidgetUtil (decorateWithClassAndBox, usageLogoWidget)

codexChild, claudeChild :: Text
codexChild = "codex"
claudeChild = "claude"

aiScratchpadStateDir :: IO FilePath
aiScratchpadStateDir = do
  stateHome <- lookupEnv "XDG_STATE_HOME"
  base <- case stateHome of
    Just dir | not (null dir) -> pure dir
    _ -> (</> ".local/state") <$> getHomeDirectory
  pure (base </> "hypr")

aiScratchpadStateFile :: FilePath
aiScratchpadStateFile = "ai-scratchpad"

aiUsageModeEnvVar :: String
aiUsageModeEnvVar = "TAFFYBAR_AI_USAGE_MODE"

-- | Keep the weekly position compact and leading while leaving all usage and
-- reset-window calculations in the library. Rows without a position retain
-- the conventional @name value@ layout.
compactUsageWindowLabel :: UsageWindowLabelParts -> Text
compactUsageWindowLabel parts =
  case usageWindowLabelPosition parts of
    Just position ->
      T.pack (show $ usageWindowPositionCurrent position)
        <> "⁄"
        <> T.pack (show $ usageWindowPositionTotal position)
        <> " "
        <> usageWindowLabelValue parts
    Nothing
      | T.null (usageWindowLabelName parts) -> usageWindowLabelValue parts
      | T.null (usageWindowLabelValue parts) -> usageWindowLabelName parts
      | otherwise -> usageWindowLabelName parts <> " " <> usageWindowLabelValue parts

-- | Read the currently selected AI scratchpad, defaulting to codex like the
-- Hyprland side does.
readActiveAIScratchpad :: IO Text
readActiveAIScratchpad = do
  dir <- aiScratchpadStateDir
  result <- try (readFile (dir </> aiScratchpadStateFile)) :: IO (Either IOException String)
  pure $ case result of
    Right contents
      | T.strip (T.pack contents) == claudeChild -> claudeChild
    _ -> codexChild

openAIUsageSection :: TaffyIO Gtk.Widget
openAIUsageSection = do
  iconWidget <- liftIO $ usageLogoWidget "openai-symbol.svg" "OpenAI usage"
  openAIUsageSectionNewWith
    iconWidget
    defaultOpenAIUsageStackConfig
      { openAIUsageStackDefaultDisplayMode = OpenAIUsageDisplayRemaining,
        openAIUsageStackLabelRenderer = const compactUsageWindowLabel
      }

anthropicUsageSection :: TaffyIO Gtk.Widget
anthropicUsageSection = do
  iconWidget <- liftIO $ usageLogoWidget "claude-symbol.svg" "Claude usage"
  anthropicUsageSectionNewWith
    iconWidget
    defaultAnthropicUsageStackConfig
      { anthropicUsageStackDefaultDisplayMode = AnthropicUsageDisplayRemaining,
        anthropicUsageStackLabelRenderer = const compactUsageWindowLabel
      }

activeAIUsageWidget :: Gtk.Widget -> Gtk.Widget -> IO Gtk.Widget
activeAIUsageWidget openAIWidget anthropicWidget = do
  stack <- Gtk.stackNew
  -- Size the provider switcher for its visible child.  The default
  -- homogeneous sizing makes Codex reserve the width of Claude's extra
  -- per-model (Fable) usage row even while that page is hidden.
  Gtk.stackSetHomogeneous stack False
  Gtk.stackAddNamed stack openAIWidget codexChild
  Gtk.stackAddNamed stack anthropicWidget claudeChild
  readActiveAIScratchpad >>= Gtk.stackSetVisibleChildName stack

  let syncVisibleChild =
        readActiveAIScratchpad
          >>= \name -> postGUIASync (Gtk.stackSetVisibleChildName stack name)

  void $ Gtk.onWidgetRealize stack $ do
    stateDir <- aiScratchpadStateDir
    createDirectoryIfMissing True stateDir
    manager <- FSNotify.startManager
    void $
      FSNotify.watchDir
        manager
        stateDir
        ((== aiScratchpadStateFile) . takeFileName . FSNotify.eventPath)
        (const syncVisibleChild)
    syncVisibleChild
    void $ Gtk.onWidgetUnrealize stack $ FSNotify.stopManager manager

  Gtk.widgetShowAll stack
  Gtk.toWidget stack

bothAIUsageWidget :: Gtk.Widget -> Gtk.Widget -> IO Gtk.Widget
bothAIUsageWidget openAIWidget anthropicWidget = do
  box <- Gtk.boxNew Gtk.OrientationHorizontal 16
  Gtk.boxPackStart box openAIWidget False False 0
  Gtk.boxPackStart box anthropicWidget False False 0
  Gtk.widgetShowAll box
  Gtk.toWidget box

-- | Show the active AI scratchpad provider by default, or both providers when
-- TAFFYBAR_AI_USAGE_MODE is set to "both".
aiUsageWidget :: TaffyIO Gtk.Widget
aiUsageWidget = do
  openAIWidget <- openAIUsageSection
  anthropicWidget <- anthropicUsageSection
  mode <- liftIO $ fmap (T.toLower . T.pack) <$> lookupEnv aiUsageModeEnvVar
  contents <-
    liftIO $
      if mode == Just "both"
        then bothAIUsageWidget openAIWidget anthropicWidget
        else activeAIUsageWidget openAIWidget anthropicWidget
  decorateWithClassAndBox "ai-usage" contents
