module TaffybarConfig.Config
  ( mkSimpleTaffyConfig,
  )
where

import TaffybarConfig.Host (compactBarHosts, smallBarHosts, tinyBarHosts)
import TaffybarConfig.Widgets (clockWidget, endWidgetsForHost, startWidgetsForHostAndBackend)
import System.Taffybar.Context (Backend)
import System.Taffybar.SimpleConfig

mkSimpleTaffyConfig :: String -> Backend -> [FilePath] -> SimpleTaffyConfig
mkSimpleTaffyConfig hostName backend cssFiles =
  defaultSimpleTaffyConfig
    { startWidgets = startWidgetsForHostAndBackend hostName backend,
      centerWidgets = [clockWidget],
      endWidgets = endWidgetsForHost hostName,
      barLevels = Nothing,
      barPosition = Top,
      widgetSpacing = 0,
      barPadding =
        if hostName `elem` tinyBarHosts
          then 0
          else
            if hostName `elem` smallBarHosts
              then 1
              else
                if hostName `elem` compactBarHosts
                  then 2
                  else 4,
      barHeight =
        if hostName `elem` tinyBarHosts
          then ScreenRatio $ 1 / 90
          else
            if hostName `elem` smallBarHosts
              then ScreenRatio $ 1 / 72
              else
                if hostName `elem` compactBarHosts
                  then ScreenRatio $ 1 / 60
                  else ScreenRatio $ 2 / 99,
      cssPaths = cssFiles
    }
