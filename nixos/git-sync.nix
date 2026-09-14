{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: let
  gitSyncServicePath = lib.makeBinPath [pkgs.coreutils pkgs.git pkgs.openssh];
  gitSyncToml = pkgs.formats.toml {};
  gmcliPackage = inputs.gmcli.packages.${pkgs.stdenv.hostPlatform.system}.default;
  gmcliViewerBase = inputs.gmcli.packages.${pkgs.stdenv.hostPlatform.system}.gmcli-viewer;
  gmcliArchiveRoot = "/home/imalison/Backups/gmcli/git-sync";
  gmcliArchiveBaseline = "${gmcliArchiveRoot}/archive";
  gmcliArchiveOutput = "${gmcliArchiveRoot}/sources/67091FDDJ0007B/archive";
  gmcliPixelTelephony = "/home/imalison/Backups/gmcli/devices/67091FDDJ0007B/2026-09-06-telephony";
  gmcliTelephonyFullOutput = "/home/imalison/Backups/gmcli/android-telephony-full";
  gmcliTelephonySnapshots = "/home/imalison/Backups/gmcli/devices";
  gmcliBackupLock = "/home/imalison/.local/state/gmcli/backup.lock";
  gmcliBackupLockDirectory = builtins.dirOf gmcliBackupLock;
  exportGmcliTelephonyFullArchive = pkgs.writeShellScript "export-gmcli-telephony-full-archive" ''
    set -euo pipefail
    ${gmcliPackage}/bin/gmcli android export-telephony \
      --adb ${pkgs.androidenv.androidPkgs.platform-tools}/bin/adb \
      --snapshot-root ${lib.escapeShellArg gmcliTelephonySnapshots} \
      --include-part-data=true
  '';
  withGmcliBackupLock = name: command:
    pkgs.writeShellScript name ''
      set -euo pipefail
      ${pkgs.coreutils}/bin/mkdir -p ${lib.escapeShellArg gmcliBackupLockDirectory}
      exec 9>${lib.escapeShellArg gmcliBackupLock}
      if ! ${pkgs.util-linux}/bin/flock --wait 1200 9; then
        echo "Timed out waiting 20 minutes for another gmcli backup job" >&2
        exit 1
      fi
      exec ${command}
    '';
  gmcliViewer = pkgs.symlinkJoin {
    name = "gmcli-viewer-with-managed-sync";
    paths = [gmcliViewerBase];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      wrapProgram "$out/bin/gmcli-viewer" \
        --set GMCLI_ARCHIVE_DIR ${lib.escapeShellArg gmcliArchiveBaseline} \
        --set GMCLI_ADDITIONAL_RELAY_DIRS ${lib.escapeShellArg gmcliArchiveOutput} \
        --set GMCLI_ADDITIONAL_TELEPHONY_DIRS ${lib.escapeShellArg gmcliPixelTelephony} \
        --set GMCLI_TELEPHONY_ARCHIVE_DIR ${lib.escapeShellArg gmcliTelephonyFullOutput}
    '';
  };
  backupGmcliTelephonyFull = withGmcliBackupLock "backup-gmcli-telephony-full" exportGmcliTelephonyFullArchive;
in {
  environment.systemPackages = [gmcliViewer];

  home-manager.users.imalison = {config, ...}: let
    gitSyncConfig = gitSyncToml.generate "git-sync-rs-config.toml" {
      defaults = {
        sync_interval = 500;
        sync_new_files = true;
        debounce = 0.5;
        min_interval = 1.0;
        initial_sync = true;
      };
      repositories = [
        {
          name = "org";
          path = config.home.homeDirectory + "/org";
          uri = "git@github.com:IvanMalison/org.git";
          watch = true;
          interval = 30;
        }
        {
          name = "password-store";
          path = config.home.homeDirectory + "/.password-store";
          uri = "git@github.com:IvanMalison/.password-store.git";
          watch = true;
        }
        {
          name = "gmcli-archive";
          path = gmcliArchiveRoot;
          uri = "git@github.com:colonelpanic8/gmcli-archive.git";
          watch = true;
          interval = 300;
          min_interval = 30.0;
        }
      ];
    };
  in {
    systemd.user.services = {
      git-sync-rs = {
        Unit = {
          Description = "Synchronize configured Git repositories";
          StartLimitIntervalSec = 300;
          StartLimitBurst = 3;
        };
        Install.WantedBy = ["default.target"];
        Service = {
          Environment = ["GIT_SYNC_TRAY=1" "PATH=${gitSyncServicePath}"];
          ExecStart = "${pkgs.git-sync-rs}/bin/git-sync-rs --config ${gitSyncConfig} watch";
          Restart = "on-failure";
          RestartSec = 5;
        };
      };
      gmcli-telephony-full-backup = {
        Unit.Description = "Back up complete Android SMS/MMS history and media";
        Service = {
          Type = "oneshot";
          ExecStart = backupGmcliTelephonyFull;
          TimeoutStartSec = "3h";
        };
      };
    };

    systemd.user.timers.gmcli-telephony-full-backup = {
      Unit.Description = "Weekly full Android SMS/MMS and media backup";
      Timer = {
        OnCalendar = "Sun *-*-* 08:00:00";
        Persistent = true;
        RandomizedDelaySec = "2h";
      };
      Install.WantedBy = ["timers.target"];
    };
  };

  home-manager.users.kat = {config, ...}: let
    gitSyncConfig = gitSyncToml.generate "kat-git-sync-rs-config.toml" {
      defaults.sync_interval = 500;
      repositories = [
        {
          name = "obsidian";
          path = config.home.homeDirectory + "/obsidian";
          uri = "git@github.com:katandtonic/obsidian.git";
          watch = true;
        }
        {
          name = "org";
          path = config.home.homeDirectory + "/org";
          uri = "ssh://gitea@1896Folsom.duckdns.org:1123/kkathuang/org.git";
          watch = true;
          interval = 180;
        }
      ];
    };
  in {
    systemd.user.services.git-sync-rs = {
      Unit.Description = "Synchronize configured Git repositories";
      Install.WantedBy = ["default.target"];
      Service = {
        Environment = ["PATH=${gitSyncServicePath}"];
        ExecStart = "${pkgs.git-sync-rs}/bin/git-sync-rs --config ${gitSyncConfig} watch";
        Restart = "on-failure";
        RestartSec = 5;
      };
    };
  };
}
