{ config, inputs, pkgs, makeEnable, ... }:
makeEnable config "myModules.taffybar" false {
  myModules.sni.enable = true;

  nixpkgs.overlays = with inputs; (
    if builtins.isList taffybar.overlays
    then taffybar.overlays
    else builtins.attrValues taffybar.overlays
  ) ++ [
    # status-notifier-item's test suite spawns `dbus-daemon`; ensure it's on PATH.
    (final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides =
          final.lib.composeExtensions (old.overrides or (_: _: {}))
          (hself: hsuper: {
            status-notifier-item = hsuper.status-notifier-item.overrideAttrs (oldAttrs: {
              checkInputs = (oldAttrs.checkInputs or []) ++ [ prev.dbus ];
              # The test suite assumes a system-wide /etc/dbus-1/session.conf,
              # which isn't present in Nix sandboxes.
              doCheck = false;
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
