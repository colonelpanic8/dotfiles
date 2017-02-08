import System.Taffybar

import System.Taffybar.MPRIS2
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.TaffyPager

import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

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
                    { useImages = True
                    , emptyWorkspace = id
                    , urgentWorkspace = id
                    , imageCount = 8
                    , workspaceGap = 0
                    , activeWorkspace = escape
                    , visibleWorkspace = escape
                    , workspaceBorder = False
                    }
      pager = taffyPagerNew pagerConfig
      mpris = mpris2New
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = systrayNew
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                        , endWidgets = [ tray, clock, mem, cpu, mpris ]
                                        , monitorNumber = 1
                                        , barPosition = Top
                                        , barHeight = 30
                                        }

-- Local Variables:
-- flycheck-ghc-args: ("-Wno-missing-signatures")
-- End:
