{ config, pkgs, inputs, lib, makeEnable, ... }:
makeEnable config "myModules.notifications-tray-icon" true {
  nixpkgs.overlays = [
    inputs.notifications-tray-icon.overlays.default
  ];

  home-manager.users.imalison = let
    notificationsTrayIcon = pkgs.haskellPackages.notifications-tray-icon;
    gmailExecStart = pkgs.writeShellScript "notifications-tray-icon-gmail" ''
      creds=$(${pkgs.pass}/bin/pass show gmail-mcp/oauth-credentials)
      client_id=$(echo "$creds" | ${pkgs.gnugrep}/bin/grep '^client_id:' | cut -d' ' -f2)
      client_secret=$(echo "$creds" | ${pkgs.gnugrep}/bin/grep '^client_secret:' | cut -d' ' -f2)
      exec ${notificationsTrayIcon}/bin/notifications-tray-icon gmail \
        --client-id "$client_id" \
        --client-secret "$client_secret"
    '';
    mkService = description: execStart: {
      Unit = {
        Description = description;
        After = [ "graphical-session-pre.target" "tray.target" ];
        PartOf = [ "graphical-session.target" ];
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
