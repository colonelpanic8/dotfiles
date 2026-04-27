{ config, pkgs, inputs, lib, makeEnable, ... }:
makeEnable config "myModules.notifications-tray-icon" true {
  nixpkgs.overlays = [
    inputs.notifications-tray-icon.overlays.default
    (final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {}))
          (hself: hsuper: {
            notifications-tray-icon = final.haskell.lib.overrideCabal
              hsuper.notifications-tray-icon
              (oldAttrs: {
                patches = (oldAttrs.patches or []) ++ [
                  ./patches/notifications-tray-icon-gmail-oauth-detached-browser.patch
                ];
              });
          });
      });
    })
  ];

  home-manager.users.imalison = { config, ... }: let
    notificationsTrayIcon = pkgs.haskellPackages.notifications-tray-icon;
    gmailExecStart = pkgs.writeShellScript "notifications-tray-icon-gmail" ''
      creds_file="${config.xdg.configHome}/gws/client_secret.json"
      client_id=$(${pkgs.jq}/bin/jq -r '.installed.client_id // .web.client_id // empty' "$creds_file")
      client_secret=$(${pkgs.jq}/bin/jq -r '.installed.client_secret // .web.client_secret // empty' "$creds_file")

      if [ -z "$client_id" ] || [ -z "$client_secret" ]; then
        echo "Failed to read Gmail OAuth client credentials from $creds_file" >&2
        exit 1
      fi

      exec ${notificationsTrayIcon}/bin/notifications-tray-icon gmail \
        --client-id "$client_id" \
        --client-secret "$client_secret"
    '';
    mkService = description: execStart: {
      Unit = {
        Description = description;
        After = [ "graphical-session.target" "tray.target" ];
        PartOf = [ "graphical-session.target" ];
        Requires = [ "tray.target" ];
      };
      Service = {
        ExecStart = execStart;
        Restart = "always";
        RestartSec = 3;
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  in {
    systemd.user.services = {
      notifications-tray-icon-github = mkService
        "GitHub notifications tray icon"
        "${notificationsTrayIcon}/bin/notifications-tray-icon github --token-pass github-token";

      notifications-tray-icon-gitea = mkService
        "Gitea notifications tray icon"
        "${notificationsTrayIcon}/bin/notifications-tray-icon gitea --url https://dev.railbird.ai --token-pass gitea-omni-token";

      notifications-tray-icon-gmail = mkService
        "Gmail notifications tray icon"
        "${gmailExecStart}";
    };
  };
}
