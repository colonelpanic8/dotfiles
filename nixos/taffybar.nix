{ config, inputs, pkgs, makeEnable, ... }:
makeEnable config "myModules.taffybar" false {
  myModules.sni.enable = true;

  nixpkgs.overlays = with inputs; (
    if builtins.isList taffybar.overlays
    then taffybar.overlays
    else builtins.attrValues taffybar.overlays
  ) ++ [
    (final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
          taffybar = prev.haskell.lib.overrideCabal hsuper.taffybar (oa: {
            postPatch = (oa.postPatch or "") + ''
              patch -p1 <<'PATCH'
              diff --git a/src/System/Taffybar/Widget/HyprlandWorkspaces.hs b/src/System/Taffybar/Widget/HyprlandWorkspaces.hs
              index 8e0e07a..a40b1a6 100644
              --- a/src/System/Taffybar/Widget/HyprlandWorkspaces.hs
              +++ b/src/System/Taffybar/Widget/HyprlandWorkspaces.hs
              @@ -27,7 +27,7 @@ import           Control.Monad.IO.Class (MonadIO(liftIO))
               import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
               import           Data.Aeson (FromJSON(..), eitherDecode', withObject, (.:), (.:?), (.!=))
               import           Data.Char (toLower)
              -import           Data.List (foldl', sortOn, stripSuffix)
              +import           Data.List (foldl', sortOn, stripPrefix)
               import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
               import qualified Data.Map.Strict as M
               import qualified Data.MultiMap as MM
              @@ -42,12 +42,11 @@ import           StatusNotifier.Tray (scalePixbufToSize)
               
               import           System.Environment.XDG.DesktopEntry
                 ( DesktopEntry
                 , deFilename
              -  , getDirectoryEntriesDefault
              -  , getImageForDesktopEntry
              +  , getDirectoryEntriesDefault
                 )
               import           System.Taffybar.Context
               import           System.Taffybar.Util
               import           System.Taffybar.Widget.Generic.AutoSizeImage (autoSizeImage)
              -import           System.Taffybar.Widget.Util (buildContentsBox, widgetSetClassGI)
              +import           System.Taffybar.Widget.Util (buildContentsBox, getImageForDesktopEntry, widgetSetClassGI)
               import           System.Taffybar.WindowIcon (getWindowIconFromClasses, pixBufFromColor)
              @@ -316,6 +315,10 @@ normalizeAppId :: String -> String
               normalizeAppId name =
                 let stripped = fromMaybe name (stripSuffix ".desktop" name)
                 in map toLower stripped
              +
              +stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
              +stripSuffix suffix xs =
              +  reverse <$> stripPrefix (reverse suffix) (reverse xs)
               
               getWindowIconFromDesktopEntryByAppId ::
                 Int32 -> String -> TaffyIO (Maybe Gdk.Pixbuf)
              PATCH
            '';
          });
        });
      });
    })
  ];

  environment.systemPackages = [
    inputs.imalison-taffybar.defaultPackage.${pkgs.stdenv.hostPlatform.system}
  ];

  home-manager.sharedModules = [
    {
      services.status-notifier-watcher.enable = true;

      # Disable kded6's statusnotifierwatcher module so it doesn't race with
      # the Haskell status-notifier-watcher for the org.kde.StatusNotifierWatcher bus name.
      xdg.configFile."kded6rc".text = ''
        [Module-statusnotifierwatcher]
        autoload=false
      '';

      services.taffybar = {
        enable = true;
        package = inputs.imalison-taffybar.defaultPackage.${pkgs.stdenv.hostPlatform.system};
      };
    }
  ];
}
