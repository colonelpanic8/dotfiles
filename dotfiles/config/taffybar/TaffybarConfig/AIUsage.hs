{-# LANGUAGE OverloadedStrings #-}

-- | A usage widget that follows the Hyprland AI scratchpad selection.
--
-- The Hyprland config (scratchpads.lua) lets SUPER+ALT+C toggle whichever AI
-- app is currently selected via rofi_ai_scratchpad.sh, which records the
-- choice in $XDG_STATE_HOME/hypr/ai-scratchpad. This widget reads the same
-- state file and shows the matching provider's usage section (OpenAI for
-- "codex", Anthropic for "claude"), switching live when the file changes.
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
      { openAIUsageStackDefaultDisplayMode = OpenAIUsageDisplayRemaining
      }

anthropicUsageSection :: TaffyIO Gtk.Widget
anthropicUsageSection = do
  iconWidget <- liftIO $ usageLogoWidget "claude-symbol.svg" "Claude usage"
  anthropicUsageSectionNewWith
    iconWidget
    defaultAnthropicUsageStackConfig
      { anthropicUsageStackDefaultDisplayMode = AnthropicUsageDisplayRemaining
      }

-- | Show usage for whichever AI app the Hyprland AI scratchpad currently
-- targets, switching live when the selection changes.
aiUsageWidget :: TaffyIO Gtk.Widget
aiUsageWidget = do
  openAIWidget <- openAIUsageSection
  anthropicWidget <- anthropicUsageSection
  stackWidget <- liftIO $ do
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
  decorateWithClassAndBox "ai-usage" stackWidget
