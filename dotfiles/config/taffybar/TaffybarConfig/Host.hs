module TaffybarConfig.Host
  ( compactBarHosts,
    cssFilesForHost,
    laptopHosts,
    smallBarHosts,
  )
where

import Data.Maybe (fromMaybe)

-- NOTE: Keep `cssPaths` to a single entrypoint file per host. GTK's
-- `cssProviderLoadFromPath` clears the provider before loading, so handing
-- Taffybar multiple files here causes only the last file to take effect.
defaultCssFiles :: [FilePath]
defaultCssFiles = ["taffybar.css"]

cssFilesByHostname :: [(String, [FilePath])]
cssFilesByHostname =
  [ ("ryzen-shine", ["ryzen-shine.css"]),
    ("strixi-minaj", ["strixi-minaj.css"])
  ]

compactBarHosts :: [String]
compactBarHosts =
  ["ryzen-shine"]

smallBarHosts :: [String]
smallBarHosts =
  ["strixi-minaj"]

laptopHosts :: [String]
laptopHosts =
  [ "adell",
    "stevie-nixos",
    "strixi-minaj",
    "jay-lenovo"
  ]

cssFilesForHost :: String -> [FilePath]
cssFilesForHost hostName =
  fromMaybe defaultCssFiles $ lookup hostName cssFilesByHostname
