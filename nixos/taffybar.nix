{ config, inputs, pkgs, makeEnable, ... }:
let
  skipTaffybarInKde = pkgs.writeShellScript "skip-taffybar-in-kde" ''
    current_desktop="''${XDG_CURRENT_DESKTOP:-}"
    desktop_session="''${DESKTOP_SESSION:-}"

    case "''${current_desktop}:''${desktop_session}" in
      *KDE*|*kde*|*Plasma*|*plasma*) exit 1 ;;
      *) exit 0 ;;
    esac
  '';
in
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
      services."status-notifier-watcher".enable = true;
      # home-manager's module defaults to nixpkgs' status-notifier-item, which can lag.
      # Point it at the pinned flake version instead.
      services."status-notifier-watcher".package = pkgs.lib.mkForce
        inputs.imalison-taffybar.packages.${pkgs.stdenv.hostPlatform.system}.status-notifier-item;

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
      systemd.user.services.taffybar.Service.ExecCondition = "${skipTaffybarInKde}";
    }
  ];
}
