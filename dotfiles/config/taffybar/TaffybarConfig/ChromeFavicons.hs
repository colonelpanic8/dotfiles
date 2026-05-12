{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TaffybarConfig.ChromeFavicons
  ( ChromeFaviconOverlayMode (..),
    ChromeFaviconConfig (..),
    defaultChromeFaviconConfig,
    chromeFaviconIconGetter,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum)
import Data.Int (Int32)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GdkPixbuf.Enums as GdkPixbuf
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    getFileSize,
    renameFile,
  )
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import qualified System.Taffybar.Information.Workspaces.Model as WorkspaceModel
import qualified System.Taffybar.Widget.Workspaces as Workspaces
import System.Taffybar.Widget.Util (loadPixbufByName)

data ChromeFaviconOverlayMode
  = FaviconWithChromeOverlay
  | ChromeWithFaviconOverlay
  deriving (Eq, Show)

data ChromeFaviconConfig = ChromeFaviconConfig
  { chromeFaviconOverlayMode :: ChromeFaviconOverlayMode,
    chromeFaviconOverlayRatio :: Double,
    chromeFaviconEnabled :: Bool
  }
  deriving (Eq, Show)

defaultChromeFaviconConfig :: ChromeFaviconConfig
defaultChromeFaviconConfig =
  ChromeFaviconConfig
    { chromeFaviconOverlayMode = FaviconWithChromeOverlay,
      chromeFaviconOverlayRatio = 0.45,
      chromeFaviconEnabled = True
    }

data BridgePayload = BridgePayload
  { payloadMappedWindowId :: Text,
    payloadFaviconURL :: Text
  }
  deriving (Eq, Show)

chromeFaviconIconGetter :: ChromeFaviconConfig -> Workspaces.WindowIconPixbufGetter
chromeFaviconIconGetter cfg =
  Workspaces.handleIconGetterException $ \size windowInfo ->
    if chromeFaviconEnabled cfg && isChromeWindow windowInfo
      then liftIO $ chromeFaviconPixbuf cfg size windowInfo
      else pure Nothing

chromeFaviconPixbuf ::
  ChromeFaviconConfig ->
  Int32 ->
  WorkspaceModel.WindowInfo ->
  IO (Maybe Gdk.Pixbuf)
chromeFaviconPixbuf cfg size windowInfo = do
  payload <- getChromeWindowInfoPayload windowInfo
  case payload of
    Just p
      | payloadMatchesWindow windowInfo p,
        validFaviconURL (payloadFaviconURL p) -> do
          mFavicon <- loadCachedFavicon size (payloadFaviconURL p)
          mChrome <- runChromeIconGetter size windowInfo
          case (mFavicon, mChrome) of
            (Just favicon, Just chromeIcon) ->
              Just <$> composeChromeFavicon cfg size favicon chromeIcon
            (Just favicon, Nothing) -> Just <$> scalePixbuf size favicon
            _ -> pure Nothing
    _ -> pure Nothing

runChromeIconGetter :: Int32 -> WorkspaceModel.WindowInfo -> IO (Maybe Gdk.Pixbuf)
runChromeIconGetter size _ =
  loadPixbufByName size "google-chrome"

payloadMatchesWindow :: WorkspaceModel.WindowInfo -> BridgePayload -> Bool
payloadMatchesWindow windowInfo payload =
  normalizedHyprlandWindowId windowInfo == Just (normalizeAddress (payloadMappedWindowId payload))

normalizedHyprlandWindowId :: WorkspaceModel.WindowInfo -> Maybe Text
normalizedHyprlandWindowId windowInfo =
  case WorkspaceModel.windowIdentity windowInfo of
    WorkspaceModel.HyprlandWindowIdentity address -> Just (normalizeAddress address)
    WorkspaceModel.X11WindowIdentity _ -> Nothing

isChromeWindow :: WorkspaceModel.WindowInfo -> Bool
isChromeWindow windowInfo =
  any looksLikeChrome (WorkspaceModel.windowClassHints windowInfo)

looksLikeChrome :: Text -> Bool
looksLikeChrome raw =
  let lowered = T.toLower raw
   in any (`T.isInfixOf` lowered) ["chrome", "chromium", "brave", "edge", "vivaldi"]

normalizeAddress :: Text -> Text
normalizeAddress address =
  let trimmed = T.strip address
   in if "0x" `T.isPrefixOf` trimmed || T.null trimmed
        then trimmed
        else "0x" <> trimmed

validFaviconURL :: Text -> Bool
validFaviconURL url =
  any (`T.isPrefixOf` url) ["https://", "http://"]

getChromeWindowInfoPayload :: WorkspaceModel.WindowInfo -> IO (Maybe BridgePayload)
getChromeWindowInfoPayload windowInfo =
  case normalizedHyprlandWindowId windowInfo of
    Just windowId -> do
      payloads <- getBridgeString "GetWindowPayloads"
      payload <- case payloads of
        Just payloadText -> extractBridgePayloadForWindow windowId payloadText
        Nothing -> pure Nothing
      case payload of
        Just value -> pure (Just value)
        Nothing -> getLastChromeWindowInfoPayload
    Nothing -> getLastChromeWindowInfoPayload

getLastChromeWindowInfoPayload :: IO (Maybe BridgePayload)
getLastChromeWindowInfoPayload = do
  payload <- getBridgeString "GetLastPayload"
  case payload of
    Just payloadText -> extractBridgePayload payloadText
    Nothing -> pure Nothing

getBridgeString :: String -> IO (Maybe String)
getBridgeString method = do
  result <-
    try @IOException $
      readProcessWithExitCode
        "busctl"
        [ "--user",
          "call",
          "org.imalison.ChromeWindowInfo",
          "/org/imalison/ChromeWindowInfo",
          "org.imalison.ChromeWindowInfo",
          method
        ]
        ""
  case result of
    Right (ExitSuccess, stdoutText, _) ->
      pure (parseBusctlString stdoutText)
    _ -> pure Nothing

parseBusctlString :: String -> Maybe String
parseBusctlString output = do
  rest <- T.stripPrefix "s " (T.strip (T.pack output))
  decodeQuotedString (T.unpack rest)

decodeQuotedString :: String -> Maybe String
decodeQuotedString raw =
  case reads raw of
    [(decoded, trailing)] | all (`elem` (" \n\t\r" :: String)) trailing -> Just decoded
    _ -> Nothing

extractBridgePayload :: String -> IO (Maybe BridgePayload)
extractBridgePayload payload = do
  let jqFilter = "[.bridge.mapped_window.window_id // \"\", .tab.favicon_url // \"\"] | @tsv"
  (code, stdoutText, _) <- readProcessWithExitCode "jq" ["-r", jqFilter] payload
  pure $
    case (code, T.splitOn "\t" (T.strip (T.pack stdoutText))) of
      (ExitSuccess, [mappedWindowId, faviconURL])
        | not (T.null mappedWindowId),
          not (T.null faviconURL) ->
            Just (BridgePayload mappedWindowId faviconURL)
      _ -> Nothing

extractBridgePayloadForWindow :: Text -> String -> IO (Maybe BridgePayload)
extractBridgePayloadForWindow windowId payloads = do
  let jqFilter = ".[$window_id] // empty | [.bridge.mapped_window.window_id // \"\", .tab.favicon_url // \"\"] | @tsv"
  (code, stdoutText, _) <-
    readProcessWithExitCode
      "jq"
      ["-r", "--arg", "window_id", T.unpack windowId, jqFilter]
      payloads
  pure $
    case (code, T.splitOn "\t" (T.strip (T.pack stdoutText))) of
      (ExitSuccess, [mappedWindowId, faviconURL])
        | not (T.null mappedWindowId),
          not (T.null faviconURL) ->
            Just (BridgePayload mappedWindowId faviconURL)
      _ -> Nothing

loadCachedFavicon :: Int32 -> Text -> IO (Maybe Gdk.Pixbuf)
loadCachedFavicon size url = do
  path <- ensureCachedFavicon url
  case path of
    Just faviconPath ->
      try @IOException (Gdk.pixbufNewFromFileAtScale faviconPath size size True) >>= \case
        Right (Just pixbuf) -> pure (Just pixbuf)
        Right Nothing -> pure Nothing
        Left _ -> pure Nothing
    Nothing -> pure Nothing

ensureCachedFavicon :: Text -> IO (Maybe FilePath)
ensureCachedFavicon url = do
  cacheRoot <- getUserCacheDir "taffybar/chrome-favicons"
  let rawDir = cacheRoot </> "raw"
  createDirectoryIfMissing True rawDir
  hash <- hashText url
  let path = rawDir </> (hash <> faviconExtension url)
  cached <- nonEmptyFileExists path
  unless cached $
    downloadFavicon url path
  exists <- nonEmptyFileExists path
  pure $ if exists then Just path else Nothing

hashText :: Text -> IO String
hashText value = do
  (code, stdoutText, _) <-
    readProcessWithExitCode "sha256sum" [] (T.unpack value)
  pure $
    if code == ExitSuccess
      then takeWhile (/= ' ') stdoutText
      else safeFileComponent value

safeFileComponent :: Text -> String
safeFileComponent =
  take 96 . map normalizeChar . T.unpack
  where
    normalizeChar c
      | isAlphaNum c = c
      | otherwise = '-'

faviconExtension :: Text -> String
faviconExtension url =
  fromMaybe ".img" $
    listToMaybe
      [ T.unpack ext
        | ext <- [".svg", ".png", ".ico", ".jpg", ".jpeg", ".webp", ".gif"] :: [Text],
          ext `T.isSuffixOf` T.toLower pathOnly
      ]
  where
    pathOnly = T.takeWhile (/= '?') url

downloadFavicon :: Text -> FilePath -> IO ()
downloadFavicon url path = do
  let tmp = path <> ".tmp"
  (code, _, _) <-
    readProcessWithExitCode
      "curl"
      [ "-fsSL",
        "--max-time",
        "10",
        "--retry",
        "1",
        "-o",
        tmp,
        T.unpack url
      ]
      ""
  when (code == ExitSuccess) $
    renameFile tmp path

nonEmptyFileExists :: FilePath -> IO Bool
nonEmptyFileExists path = do
  exists <- doesFileExist path
  if exists
    then (> 0) <$> getFileSize path
    else pure False

composeChromeFavicon ::
  ChromeFaviconConfig ->
  Int32 ->
  Gdk.Pixbuf ->
  Gdk.Pixbuf ->
  IO Gdk.Pixbuf
composeChromeFavicon cfg size favicon chromeIcon = do
  let (baseSource, overlaySource) =
        case chromeFaviconOverlayMode cfg of
          FaviconWithChromeOverlay -> (favicon, chromeIcon)
          ChromeWithFaviconOverlay -> (chromeIcon, favicon)
  base <- scalePixbuf size baseSource
  result <- fromMaybe base <$> Gdk.pixbufCopy base
  baseWidth <- Gdk.pixbufGetWidth result
  baseHeight <- Gdk.pixbufGetHeight result
  let baseSize = max 1 (min baseWidth baseHeight)
      overlaySize =
        max 1 $
          min baseSize $
            round (fromIntegral baseSize * chromeFaviconOverlayRatio cfg)
      overlayX = baseWidth - overlaySize
      overlayY = baseHeight - overlaySize
  overlay <- scalePixbuf overlaySize overlaySource
  Gdk.pixbufComposite
    overlay
    result
    overlayX
    overlayY
    overlaySize
    overlaySize
    (fromIntegral overlayX)
    (fromIntegral overlayY)
    1
    1
    GdkPixbuf.InterpTypeBilinear
    255
  pure result

scalePixbuf :: Int32 -> Gdk.Pixbuf -> IO Gdk.Pixbuf
scalePixbuf size pixbuf =
  fromMaybe pixbuf <$> Gdk.pixbufScaleSimple pixbuf size size GdkPixbuf.InterpTypeBilinear
