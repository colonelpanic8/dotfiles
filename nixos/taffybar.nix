{ config, inputs, lib, pkgs, makeEnable, ... }:
let
  system = pkgs.stdenv.hostPlatform.system;
  taffybarPackage = inputs.imalison-taffybar.defaultPackage.${system};
  taffybarStart = pkgs.writeShellScript "taffybar-start" ''
    runtime_dir="''${XDG_RUNTIME_DIR:-/run/user/$(${pkgs.coreutils}/bin/id -u)}"

    if [ -n "''${DISPLAY:-}" ] && [ "''${XDG_SESSION_TYPE:-}" != "wayland" ]; then
      unset WAYLAND_DISPLAY
      unset HYPRLAND_INSTANCE_SIGNATURE
      exec ${taffybarPackage}/bin/taffybar "$@"
    fi

    if [ "''${XDG_SESSION_TYPE:-}" = "wayland" ] || [ -n "''${WAYLAND_DISPLAY:-}" ]; then
      if [ -z "''${WAYLAND_DISPLAY:-}" ] || [ ! -S "$runtime_dir/$WAYLAND_DISPLAY" ]; then
        for socket in "$runtime_dir"/wayland-*; do
          case "$socket" in
            *.lock) continue ;;
          esac

          if [ -S "$socket" ]; then
            socket_name="''${socket##*/}"
            echo "taffybar-start: correcting WAYLAND_DISPLAY=''${WAYLAND_DISPLAY:-<unset>} to $socket_name" >&2
            export WAYLAND_DISPLAY="$socket_name"
            break
          fi
        done
      fi
    fi

    current_desktop="''${XDG_CURRENT_DESKTOP:-}"
    desktop_session="''${DESKTOP_SESSION:-}"
    is_hyprland=0
    case "''${current_desktop}:''${desktop_session}" in
      *Hyprland*|*hyprland*) is_hyprland=1 ;;
    esac

    if [ -n "''${HYPRLAND_INSTANCE_SIGNATURE:-}" ] && [ ! -S "$runtime_dir/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket.sock" ]; then
      echo "taffybar-start: unsetting stale HYPRLAND_INSTANCE_SIGNATURE=$HYPRLAND_INSTANCE_SIGNATURE" >&2
      unset HYPRLAND_INSTANCE_SIGNATURE
    fi

    if [ "$is_hyprland" = 1 ]; then
      if [ -z "''${HYPRLAND_INSTANCE_SIGNATURE:-}" ]; then
        for socket in "$runtime_dir"/hypr/*/.socket.sock; do
          if [ -S "$socket" ]; then
            socket_dir="''${socket%/.socket.sock}"
            signature="''${socket_dir##*/}"
            echo "taffybar-start: correcting HYPRLAND_INSTANCE_SIGNATURE=''${HYPRLAND_INSTANCE_SIGNATURE:-<unset>} to $signature" >&2
            export HYPRLAND_INSTANCE_SIGNATURE="$signature"
            break
          fi
        done
      fi
    fi

    exec ${taffybarPackage}/bin/taffybar "$@"
  '';
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
    taffybarPackage
  ];

  home-manager.sharedModules = [
    ({ lib, ... }: {
      services."status-notifier-watcher".enable = true;
      # home-manager's module defaults to nixpkgs' status-notifier-item, which can lag.
      # Point it at the pinned flake version instead.
      services."status-notifier-watcher".package = pkgs.lib.mkForce
        inputs.imalison-taffybar.packages.${system}.status-notifier-item;

      # Disable kded6's statusnotifierwatcher module so it doesn't race with
      # the Haskell status-notifier-watcher for the org.kde.StatusNotifierWatcher bus name.
      xdg.configFile."kded6rc".text = ''
        [Module-statusnotifierwatcher]
        autoload=false
      '';

      services.taffybar = {
        enable = true;
        package = taffybarPackage;
      };
      xdg.configFile."systemd/user/taffybar.service".force = true;
      home.activation.removeStaleTaffybarOverride =
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          rm -f "$HOME/.config/systemd/user/taffybar.service.d/override.conf"
          rmdir --ignore-fail-on-non-empty "$HOME/.config/systemd/user/taffybar.service.d" 2>/dev/null || true
        '';
      systemd.user.services.taffybar.Service = {
        ExecCondition = "${skipTaffybarInKde}";
        ExecStart = lib.mkForce "${taffybarStart}";
        # Temporary startup debugging: keep a plain-text log outside journald so
        # the next login/startup leaves easy-to-inspect tray traces behind.
        StandardOutput = "append:/tmp/taffybar-service.log";
        StandardError = "append:/tmp/taffybar-service.log";
      };
    })
  ];
}
