{
  config,
  inputs,
  lib,
  pkgs,
  makeEnable,
  ...
}: let
  cfg = config.myModules.sni;
  isFull = cfg.profile == "full";
  system = pkgs.stdenv.hostPlatform.system;
  kanshiSniPackage = inputs.kanshi-sni.packages.${system}.default;
  # appConfig.json is owned by the app (it rejects partial files), so settings
  # are applied through its CLI once it is up. systemd owns startup and Nix
  # owns updates; tailscale0 lets peers be paired by tailnet address.
  crosspasteConfigure = pkgs.writeShellScript "crosspaste-configure" ''
    crosspaste() { ${pkgs.crosspaste}/bin/crosspaste --no-start "$@"; }

    for _ in $(seq 1 60); do
      crosspaste status >/dev/null 2>&1 && break
      sleep 1
    done

    crosspaste config set enableAutoStartUp false >/dev/null
    crosspaste config set autoDownloadUpdate false >/dev/null
    rm -f "''${XDG_CONFIG_HOME:-$HOME/.config}/autostart/crosspaste.desktop"

    if [ -e /sys/class/net/tailscale0 ]; then
      interfaces="$(crosspaste --json config | ${pkgs.jq}/bin/jq -c '.useNetworkInterfaces | fromjson')"
      if ! ${pkgs.jq}/bin/jq -e 'index("tailscale0")' <<<"$interfaces" >/dev/null; then
        crosspaste config set useNetworkInterfaces \
          "$(${pkgs.jq}/bin/jq -c '. + ["tailscale0"]' <<<"$interfaces")" >/dev/null
      fi
    fi
  '';
  enabledModule = makeEnable config "myModules.sni" true {
    systemd.user.services.blueman-applet.serviceConfig.ExecStart = lib.mkIf isFull (lib.mkForce [
      ""
      "${pkgs.blueman}/bin/blueman-applet"
    ]);

    home-manager.sharedModules = [
      ({lib, ...}: {
        systemd.user.services.kanshi-sni = lib.mkIf isFull {
          Unit = {
            Description = "kanshi-sni tray app";
            After = ["graphical-session.target" "tray.target" "kanshi.service"];
            PartOf = ["graphical-session.target" "kanshi.service"];
            Requires = ["tray.target"];
            Wants = ["kanshi.service"];
          };
          Service = {
            ExecStart = "${kanshiSniPackage}/bin/kanshi-sni";
            Restart = "always";
            RestartSec = 3;
          };
          Install = {
            WantedBy = ["graphical-session.target"];
          };
        };

        systemd.user.services.crosspaste = {
          Unit = {
            Description = "CrossPaste tray app";
            After = ["graphical-session.target" "tray.target"];
            PartOf = ["graphical-session.target"];
            Requires = ["tray.target"];
          };
          Service = {
            ExecStart = "${pkgs.crosspaste}/bin/crosspaste-desktop";
            ExecStartPost = "-${crosspasteConfigure}";
            Restart = "on-failure";
            RestartSec = 3;
          };
          Install = {
            WantedBy = ["graphical-session.target"];
          };
        };

        services.kdeconnect = lib.mkIf isFull {
          enable = true;
          indicator = true;
        };

        home.activation.disableKdeConnectBluetooth = lib.hm.dag.entryAfter ["writeBoundary"] (
          lib.optionalString isFull ''
            ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 \
              --file kdeconnect/config \
              --group General \
              --key disabled_providers \
              'BluetoothLinkProvider,AsyncLinkProvider'
          ''
        );

        services.network-manager-applet.enable = true;

        # Disable the XDG autostart for nm-applet since we're managing it via systemd.
        # The XDG autostart races with the systemd service and doesn't use --indicator.
        xdg.configFile."autostart/nm-applet.desktop".text = ''
          [Desktop Entry]
          Hidden=true
        '';

        services.udiskie = {
          enable = true;
          tray = "always";
        };

        services.pasystray.enable = isFull;
      })
    ];
  };
in
  enabledModule
  // {
    options = lib.recursiveUpdate enabledModule.options {
      myModules.sni.profile = lib.mkOption {
        type = lib.types.enum ["minimal" "full"];
        default = "full";
        description = ''
          Select core tray services (NetworkManager, udiskie, CrossPaste) or
          the complete workstation tray set.
        '';
      };
    };
  }
