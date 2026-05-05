module TaffybarConfig.Config
  ( mkSimpleTaffyConfig,
  )
where

import TaffybarConfig.Host (compactBarHosts, smallBarHosts)
import TaffybarConfig.Widgets (clockWidget, endWidgetsForHost, startWidgetsForBackend)
import System.Taffybar.Context (Backend)
import System.Taffybar.SimpleConfig

mkSimpleTaffyConfig :: String -> Backend -> [FilePath] -> SimpleTaffyConfig
mkSimpleTaffyConfig hostName backend cssFiles =
  defaultSimpleTaffyConfig
    { startWidgets = startWidgetsForBackend backend,
      centerWidgets = [clockWidget],
      endWidgets = endWidgetsForHost hostName,
      barLevels = Nothing,
      barPosition = Top,
      widgetSpacing = 0,
      barPadding =
        if hostName `elem` smallBarHosts
          then 1
          else
            if hostName `elem` compactBarHosts
              then 2
              else 4,
      barHeight =
        if hostName `elem` smallBarHosts
          then ScreenRatio $ 1 / 48
          else
            if hostName `elem` compactBarHosts
              then ScreenRatio $ 1 / 40
              else ScreenRatio $ 1 / 33,
      cssPaths = cssFiles
    }
