module Main where

import qualified Data.Word8 as W
import System.Information.CPU
import System.Information.Memory
import System.Taffybar
import System.Taffybar.IconImages
import System.Taffybar.MPRIS2
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.WorkspaceHUD

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

zero :: W.Word8
zero = fromIntegral 0

alwaysTransparent _ _ =
  IIColor $ (fromIntegral 0xFF, fromIntegral 0, zero, fromIntegral 0xFF)

myGetIconInfo =
  windowTitleClassIconGetter False alwaysTransparent

main = do
  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                                  , graphLabel = Just "mem"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Just "cpu"
                                  }
  let clock = textClockNew Nothing "%a %b %_d %r" 1
      pagerConfig = defaultPagerConfig
      mpris = mpris2New
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = systrayNew
      hudConfig = defaultWorkspaceHUDConfig { underlineHeight = 3
                                            , minWSWidgetSize = Nothing
                                            , minIcons = 3
                                            , getIconInfo = myGetIconInfo
                                            , updateIconsOnTitleChange = False
                                            , widgetBuilder = buildBorderButtonController
                                            -- , showWorkspaceFn = hideEmpty
                                            }
      hudPagerConfig = hudFromPagerConfig pagerConfig
      hud = taffyPagerHUDNew pagerConfig hudConfig

  defaultTaffybar defaultTaffybarConfig { startWidgets = [ hud ]
                                        , endWidgets = [ tray, clock, mem, cpu, mpris ]
                                        , monitorNumber = 1
                                        , barPosition = Top
                                        , barHeight = 30
                                        }

-- Local Variables:
-- flycheck-ghc-args: ("-Wno-missing-signatures")
-- End:
