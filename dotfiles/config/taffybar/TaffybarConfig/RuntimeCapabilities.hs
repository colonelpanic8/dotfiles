module TaffybarConfig.RuntimeCapabilities
  ( RuntimeCapabilities (..),
    getRuntimeCapabilities,
    startRuntimeCapabilityMonitoring,
  )
where

import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, readTChan)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Either (rights)
import Data.Maybe (isJust)
import System.Taffybar.Context
  ( TaffyIO,
    getStateDefault,
    refreshTaffyWindows,
    taffyFork,
  )
import System.Taffybar.Information.Backlight
  ( getBacklightInfo,
    getBacklightInfoChan,
  )
import System.Taffybar.Information.Battery
  ( BatteryInfo (..),
    BatteryType (BatteryTypeBatteryType),
    getBatteryInfo,
    getBatteryPaths,
    getDisplayBatteryChan,
  )
import System.Taffybar.Information.CPUPower
  ( CPUPowerInfo (..),
    getCPUPowerInfoChan,
    readCPUPowerInfo,
  )

data RuntimeCapabilities = RuntimeCapabilities
  { runtimeHasBattery :: Bool,
    runtimeHasBacklight :: Bool,
    runtimeHasCPUPower :: Bool
  }
  deriving (Eq, Show)

newtype RuntimeCapabilitiesVar = RuntimeCapabilitiesVar (MVar RuntimeCapabilities)

isSystemBattery :: BatteryInfo -> Bool
isSystemBattery info =
  batteryType info == BatteryTypeBatteryType
    && batteryPowerSupply info
    && batteryIsPresent info

detectBattery :: TaffyIO Bool
detectBattery = do
  pathsResult <- getBatteryPaths
  case pathsResult of
    Left _ -> pure False
    Right paths -> do
      infos <- rights <$> mapM getBatteryInfo paths
      pure $ any isSystemBattery infos

detectRuntimeCapabilities :: TaffyIO RuntimeCapabilities
detectRuntimeCapabilities = do
  hasBattery <- detectBattery
  hasBacklight <- liftIO $ isJust <$> getBacklightInfo Nothing
  hasCPUPower <- liftIO $ isJust . cpuPackagePowerWatts <$> readCPUPowerInfo
  pure $
    RuntimeCapabilities
      { runtimeHasBattery = hasBattery,
        runtimeHasBacklight = hasBacklight,
        runtimeHasCPUPower = hasCPUPower
      }

getRuntimeCapabilitiesVar :: TaffyIO RuntimeCapabilitiesVar
getRuntimeCapabilitiesVar =
  getStateDefault $ do
    capabilities <- detectRuntimeCapabilities
    RuntimeCapabilitiesVar <$> liftIO (newMVar capabilities)

getRuntimeCapabilities :: TaffyIO RuntimeCapabilities
getRuntimeCapabilities = do
  RuntimeCapabilitiesVar capabilitiesVar <- getRuntimeCapabilitiesVar
  liftIO $ readMVar capabilitiesVar

duplicateChannel :: TChan a -> IO (TChan a)
duplicateChannel = atomically . dupTChan

updateCapabilities ::
  MVar RuntimeCapabilities ->
  (RuntimeCapabilities -> RuntimeCapabilities) ->
  TaffyIO ()
updateCapabilities capabilitiesVar update = do
  changed <-
    liftIO $
      modifyMVar capabilitiesVar $ \old -> do
        let new = update old
        pure (new, new /= old)
  when changed refreshTaffyWindows

startRuntimeCapabilityMonitoring :: TaffyIO ()
startRuntimeCapabilityMonitoring = do
  RuntimeCapabilitiesVar capabilitiesVar <- getRuntimeCapabilitiesVar
  batteryChan <- getDisplayBatteryChan >>= liftIO . duplicateChannel
  backlightChan <- getBacklightInfoChan Nothing >>= liftIO . duplicateChannel
  cpuPowerChan <- getCPUPowerInfoChan 1 >>= liftIO . duplicateChannel

  taffyFork $ forever $ do
    info <- liftIO $ atomically $ readTChan batteryChan
    updateCapabilities capabilitiesVar $ \capabilities ->
      capabilities {runtimeHasBattery = isSystemBattery info}

  taffyFork $ forever $ do
    info <- liftIO $ atomically $ readTChan backlightChan
    updateCapabilities capabilitiesVar $ \capabilities ->
      capabilities {runtimeHasBacklight = isJust info}

  void $ taffyFork $ forever $ do
    info <- liftIO $ atomically $ readTChan cpuPowerChan
    updateCapabilities capabilitiesVar $ \capabilities ->
      capabilities {runtimeHasCPUPower = isJust $ cpuPackagePowerWatts info}
