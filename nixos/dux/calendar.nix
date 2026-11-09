# Dux Calendar (dcal): the calendar daemon behind the Dux Shell calendar.
# Once dcal is up the shell reads events and tasks from it instead of khal, so
# it carries the vdirsyncer Google feeds (google-calendar.nix) as a local
# account, plus org-agenda-api TODOs through the org-agenda-dcal provider.
# The API password stays in agenix and is read only when the provider runs.
{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myModules.dux-calendar;
  system = pkgs.stdenv.hostPlatform.system;
  orgProvider = inputs.org-agenda-api.packages.${system}.dcal-provider;
  # Same build Dux Shell installs, so the two don't collide in home.packages.
  quickshellPackage = inputs.quickshell-tray.packages.${system}.default;
in {
  options.myModules.dux-calendar.enable =
    lib.mkEnableOption "Dux Calendar with org-agenda-api and Google calendars"
    // {
      default = config.myModules.dux-shell.enable && config.myUsers.imalison.has.full;
    };

  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [inputs.dux-calendar.homeModules.dank-calendar];

    home-manager.users.imalison = {config, ...}: let
      dcal = config.programs.dank-calendar.package;
      googleCalendars = "${config.xdg.dataHome}/calendars/google";
    in {
      age.secrets.org-api-auth-password.file = ../secrets/org-api-auth-password.age;

      programs.dank-calendar = {
        enable = true;
        systemd.enable = true;
        quickshell.package = quickshellPackage;
        externalAccounts.org-agenda = {
          displayName = "Org agenda";
          command = ["${orgProvider}/bin/org-agenda-dcal" "--calendar-name" "Org agenda"];
          env = {
            ORG_AGENDA_API_URL = "https://org-agenda-api.rocket-sense.duckdns.org";
            ORG_AGENDA_API_USER = "imalison";
            # An AUTH_PASSWORD=... env file; the path expands $XDG_RUNTIME_DIR at run time.
            ORG_AGENDA_API_PASSWORD_COMMAND = "${pkgs.gnused}/bin/sed -n 's/^AUTH_PASSWORD=//p' \"${config.age.secrets.org-api-auth-password.path}\"";
          };
        };
      };

      systemd.user.services.dcal.Unit = {
        After = ["agenix.service"];
        Wants = ["agenix.service"];
      };

      # Read-only: vdirsyncer owns the tree and "remote wins". Idempotent; runs
      # before the daemon starts and again after every vdirsyncer sync, so a
      # fresh login registers the feeds once the first sync lands (and the
      # daemon picks up each sync right away).
      systemd.user.services.dcal-google-calendars = lib.mkIf config.programs.khal.enable {
        Unit = {
          Description = "Register vdirsyncer Google calendars with Dux Calendar";
          Before = ["dcal.service"];
          After = ["vdirsyncer.service"];
        };
        Service = {
          Type = "oneshot";
          ExecCondition = toString (pkgs.writeShellScript "google-calendars-synced" ''
            [[ -n $(${pkgs.findutils}/bin/find ${googleCalendars} -name '*.ics' -print -quit 2>/dev/null) ]]
          '');
          ExecStart = "${dcal}/bin/dcal account add local ${googleCalendars} --name Google --read-only";
        };
        Install.WantedBy = ["dcal.service" "vdirsyncer.service"];
      };
    };
  };
}
