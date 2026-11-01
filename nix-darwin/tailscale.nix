{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myModules.tailscale;
  tailscale = "${config.services.tailscale.package}/bin/tailscale";
  prefs = cfg.enrollmentEnvironment;
  operator = prefs.OPERATOR or config.system.primaryUser;
  preferenceArgs =
    [
      "--accept-dns=${prefs.ACCEPT_DNS or "true"}"
      "--hostname=${config.networking.hostName}"
    ]
    ++ lib.optional (operator != "") "--operator=${operator}";
in {
  imports = [../nix-shared/tailscale];

  config = lib.mkIf cfg.enable {
    services.tailscale.enable = true;
    launchd.daemons.tailscaled.serviceConfig.KeepAlive = true;

    # tailscaled needs writable resolver files rather than Nix store symlinks.
    environment.etc."resolver/ts.net".enable = false;

    # The GUI client and this daemon have independent identities and VPNs.
    system.activationScripts.preActivation.text = lib.mkBefore ''
      if [ -d /Applications/Tailscale.app ]; then
        echo >&2 "Remove Tailscale.app, empty the Trash, and reboot before switching to the Nix-managed Tailscale daemon."
        echo >&2 "See nix-darwin/README.md for migration and login instructions."
        exit 1
      fi
    '';

    # Preserve the existing macOS preference reconciliation, independently of
    # whether a machine chooses an automatic enrollment profile.
    launchd.daemons.tailscale-autoconnect = {
      script = ''
        set -euo pipefail
        state=""
        for _ in $(${pkgs.coreutils}/bin/seq 1 30); do
          state="$(${tailscale} status --json 2>/dev/null | ${pkgs.jq}/bin/jq -r '.BackendState // empty' || true)"
          if [ -n "$state" ] && [ "$state" != "NoState" ]; then
            break
          fi
          sleep 2
        done
        if [ -z "$state" ] || [ "$state" = "NoState" ]; then
          echo "Timed out waiting for tailscaled" >&2
          exit 1
        fi
        ${tailscale} set ${lib.escapeShellArgs preferenceArgs}
      '';
      serviceConfig = {
        RunAtLoad = true;
        StartInterval = 300;
        StandardOutPath = "/var/log/tailscale-autoconnect.log";
        StandardErrorPath = "/var/log/tailscale-autoconnect.err.log";
      };
    };
    launchd.daemons.tailscale-enroll = lib.mkIf (cfg.enrollment != null) {
      script = "exec ${pkgs.bash}/bin/bash ${../nix-shared/tailscale/enroll.sh}";
      serviceConfig = {
        EnvironmentVariables = cfg.enrollmentEnvironment;
        RunAtLoad = true;
        StartInterval = 300;
        StandardOutPath = "/var/log/tailscale-enroll.log";
        StandardErrorPath = "/var/log/tailscale-enroll.err.log";
      };
    };
  };
}
