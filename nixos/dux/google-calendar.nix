# Read-only Google Calendar for khal (and so the Dux Shell calendar), pulled
# from each calendar's "Secret address in iCal format". No OAuth: the feed URLs
# live in an agenix secret, one "<name> <url>" line per calendar.
{
  config,
  lib,
  pkgs,
  ...
}: let
  # Must match the names in the secret.
  calendars = ["primary" "akumal" "family" "jj_cleaning" "org_mode"];
in {
  config = lib.mkIf (config.myModules.dux-shell.enable && config.myUsers.imalison.has.full) {
    home-manager.users.imalison = {config, ...}: let
      urlsFile = config.age.secrets.google-calendar-ics-urls.path;
      basePath = "${config.xdg.dataHome}/calendars/google";
      feedUrl = pkgs.writeShellScript "google-calendar-feed-url" ''
        exec ${pkgs.gawk}/bin/awk -v name="$1" '$1 == name { print $2 }' "${urlsFile}"
      '';
    in {
      age.secrets.google-calendar-ics-urls.file = ../secrets/google-calendar-ics-urls.age;

      accounts.calendar.basePath = basePath;
      accounts.calendar.accounts = lib.genAttrs calendars (name: {
        local = {
          type = "filesystem";
          fileExt = ".ics";
        };
        remote.type = "http";
        vdirsyncer = {
          enable = true;
          urlCommand = ["${feedUrl}" name];
          collections = null;
          conflictResolution = "remote wins";
        };
        khal = {
          enable = true;
          readOnly = true;
          type = "calendar";
        };
      });

      programs.khal.enable = true;
      programs.vdirsyncer.enable = true;
      services.vdirsyncer = {
        enable = true;
        frequency = "*:0/15";
      };

      systemd.user.services.vdirsyncer = {
        Unit = {
          After = ["agenix.service"];
          Wants = ["agenix.service"];
        };
        # The HM unit never runs discover, which sync refuses to skip.
        Service.ExecStartPre = [
          "${pkgs.coreutils}/bin/mkdir -p ${lib.concatMapStringsSep " " (name: "${basePath}/${name}") calendars}"
          "${config.services.vdirsyncer.package}/bin/vdirsyncer discover"
        ];
      };
      systemd.user.timers.vdirsyncer.Timer.OnStartupSec = "1min";
    };
  };
}
