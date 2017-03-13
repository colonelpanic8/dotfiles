{-# LANGUAGE OverloadedStrings #-}
module ToggleMonitor (
  handleToggleRequests,
  toggleableMonitors
) where

import           Control.Concurrent
import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Map as M
import           Data.Maybe
import           System.Taffybar
import           Text.Read hiding (get)
import           Web.Scotty
import           XMonad.Core ( whenJust )

toggleableMonitors :: MV.MVar (M.Map Int Bool) -> Int -> TaffybarConfig -> IO (Maybe TaffybarConfig)
toggleableMonitors enabledVar monNumber cfg = do
  numToEnabled <- MV.readMVar enabledVar
  let enabled = fromMaybe True $ M.lookup monNumber numToEnabled
  return $ if enabled then Nothing else Just cfg

handleToggleRequests :: MV.MVar (M.Map Int Bool) -> IO () -> IO ()
handleToggleRequests enabledVar refreshTaffyWindows = do
  let toggleTaffyOnMon fn mon = do
        MV.modifyMVar_ enabledVar $ \numToEnabled -> do
          let current = fromMaybe False $ M.lookup mon numToEnabled
          return $ M.insert mon (fn current) numToEnabled
        refreshTaffyWindows
      runScotty =
        scotty 3000 $ do
          get "/toggle/:monNum" $ do
            num <- param "monNum"
            liftIO $
              whenJust (readMaybe num :: Maybe Int) $ toggleTaffyOnMon not
          get "on/:monNum" $ do
            num <- param "monNum"
            liftIO $
              whenJust (readMaybe num :: Maybe Int) $
              toggleTaffyOnMon $ const True
          get "off/:monNum" $ do
            num <- param "monNum"
            liftIO $
              whenJust (readMaybe num :: Maybe Int) $
              toggleTaffyOnMon $ const False
  void $ forkIO runScotty
