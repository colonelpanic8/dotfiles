{-# LANGUAGE OverloadedStrings #-}

module TaffybarConfig.Workspaces
  ( workspaceLabelSetter,
    workspaceWindowIconGetter,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Int (Int32)
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import System.Taffybar.Context
  ( Backend (BackendX11),
    TaffyIO,
    backend,
    runX11Def,
  )
import System.Taffybar.Information.EWMHDesktopInfo (WorkspaceId (..))
import qualified System.Taffybar.Information.Workspaces.Model as WorkspaceModel
import System.Taffybar.Information.X11DesktopInfo
import System.Taffybar.Util (getPixbufFromFilePath, maybeTCombine, (<|||>))
import System.Taffybar.Widget.Util (loadPixbufByName)
import qualified System.Taffybar.Widget.Workspaces as Workspaces
import System.Taffybar.WindowIcon (pixBufFromColor)

x11FullWorkspaceNames :: X11Property [(WorkspaceId, String)]
x11FullWorkspaceNames =
  go <$> readAsListOfString Nothing "_NET_DESKTOP_FULL_NAMES"
  where
    go = zip [WorkspaceId i | i <- [0 ..]]

remapNSP :: String -> String
remapNSP "NSP" = "S"
remapNSP n = n

workspaceLabelSetter :: WorkspaceModel.WorkspaceInfo -> TaffyIO String
workspaceLabelSetter workspace = do
  backendType <- asks backend
  let identity = WorkspaceModel.workspaceIdentity workspace
      fallbackLabel = remapNSP $ T.unpack (WorkspaceModel.workspaceName identity)
  case (backendType, WorkspaceModel.workspaceNumericId identity) of
    (BackendX11, Just workspaceId) -> do
      fullNames <- runX11Def [] x11FullWorkspaceNames
      return $ remapNSP $ fromMaybe fallbackLabel (lookup (WorkspaceId workspaceId) fullNames)
    _ -> return fallbackLabel

iconRemap :: [(Text, [Text])]
iconRemap =
  [ ("spotify", ["spotify-client", "spotify"])
  ]

iconRemapMap :: M.Map Text [Text]
iconRemapMap =
  M.fromList [(T.toLower k, v) | (k, v) <- iconRemap]

lookupIconRemap :: Text -> [Text]
lookupIconRemap name = fromMaybe [] $ M.lookup (T.toLower name) iconRemapMap

iconNameVariants :: Text -> [Text]
iconNameVariants raw =
  let lower = T.toLower raw
      stripped = fromMaybe lower (T.stripSuffix ".desktop" lower)
      suffixes = ["-gtk", "-client", "-desktop"]
      stripSuffixes name =
        let variants = mapMaybe (`T.stripSuffix` name) suffixes
         in nub $ variants ++ [name]
      baseNames = stripSuffixes stripped ++ [raw]
      toDash c
        | c == ' ' || c == '_' || c == '.' || c == '/' = '-'
        | otherwise = c
      toUnderscore c
        | c == ' ' || c == '-' || c == '.' || c == '/' = '_'
        | otherwise = c
      variantsFor name =
        let dotted =
              case T.splitOn "." name of
                [] -> name
                xs -> last xs
            dashed = T.map toDash name
            dashedDotted = T.map toDash dotted
            underscored = T.map toUnderscore name
            underscoredDotted = T.map toUnderscore dotted
         in [dotted, dashed, dashedDotted, underscored, underscoredDotted, name]
   in nub $ concatMap variantsFor baseNames

workspaceIconCandidates :: WorkspaceModel.WindowInfo -> [Text]
workspaceIconCandidates windowData =
  let baseNames = WorkspaceModel.windowClassHints windowData
      remapped = concatMap lookupIconRemap baseNames
      remappedExpanded = concatMap iconNameVariants remapped
      baseExpanded = concatMap iconNameVariants baseNames
   in nub (remappedExpanded ++ baseExpanded)

isPathCandidate :: Text -> Bool
isPathCandidate name =
  T.isInfixOf "/" name
    || any (`T.isSuffixOf` name) [".png", ".svg", ".xpm"]

workspaceCandidateInfo :: Text -> WorkspaceModel.WindowInfo
workspaceCandidateInfo name =
  WorkspaceModel.WindowInfo
    { WorkspaceModel.windowIdentity = WorkspaceModel.HyprlandWindowIdentity "",
      WorkspaceModel.windowTitle = "",
      WorkspaceModel.windowClassHints = [name],
      WorkspaceModel.windowPosition = Nothing,
      WorkspaceModel.windowUrgent = False,
      WorkspaceModel.windowActive = False,
      WorkspaceModel.windowMinimized = False
    }

workspaceIconFromCandidate :: Int32 -> Text -> TaffyIO (Maybe Gdk.Pixbuf)
workspaceIconFromCandidate size name
  | isPathCandidate name =
      liftIO $ getPixbufFromFilePath (T.unpack name)
  | otherwise =
      maybeTCombine
        (Workspaces.getWindowIconPixbufFromDesktopEntry size (workspaceCandidateInfo name))
        (liftIO $ loadPixbufByName size name)

workspaceManualIconGetter :: Workspaces.WindowIconPixbufGetter
workspaceManualIconGetter =
  Workspaces.handleIconGetterException $ \size windowData ->
    foldl maybeTCombine (return Nothing) $
      map (workspaceIconFromCandidate size) (workspaceIconCandidates windowData)

fallbackIconPixbuf :: Int32 -> TaffyIO (Maybe Gdk.Pixbuf)
fallbackIconPixbuf size = do
  let fallbackNames =
        [ "application-x-executable",
          "application",
          "image-missing",
          "gtk-missing-image",
          "dialog-question",
          "utilities-terminal",
          "system-run",
          "window"
        ]
      tryNames =
        foldl
          maybeTCombine
          (return Nothing)
          (map (liftIO . loadPixbufByName size) fallbackNames)
  result <- tryNames
  case result of
    Just _ -> return result
    Nothing -> Just <$> pixBufFromColor size 0x5f5f5fff

workspaceFallbackIcon :: Workspaces.WindowIconPixbufGetter
workspaceFallbackIcon size _ =
  fallbackIconPixbuf size

workspaceWindowIconGetter :: Workspaces.WindowIconPixbufGetter
workspaceWindowIconGetter =
  workspaceManualIconGetter
    <|||> Workspaces.getWindowIconPixbufFromChrome
    <|||> Workspaces.defaultGetWindowIconPixbuf
    <|||> workspaceFallbackIcon
