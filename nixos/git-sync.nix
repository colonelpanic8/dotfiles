{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: let
  gitSyncServicePath = lib.makeBinPath [pkgs.coreutils pkgs.git pkgs.openssh];
  # AI chat-history sync (Claude Code + Codex) is rolled out machine-by-machine;
  # each new machine needs its existing history merged into the repo first (see
  # github.com/colonelpanic8/claude-history and .../codex-history).
  aiHistoryHosts = ["ryzen-shine" "railbird-sf" "jay-lenovo" "strixi-minaj"];
  syncAiHistory = builtins.elem config.networking.hostName aiHistoryHosts;
  gmcliPackage = inputs.gmcli.packages.${pkgs.stdenv.hostPlatform.system}.default;
  gmcliViewerBase = inputs.gmcli.packages.${pkgs.stdenv.hostPlatform.system}.gmcli-viewer;
  gmcliCookiePython = pkgs.python3.withPackages (ps: [ps.browser-cookie3]);
  gmcliArchiveRoot = "/home/imalison/Backups/gmcli/git-sync";
  gmcliArchiveOutput = "${gmcliArchiveRoot}/archive";
  gmcliTelephonyOutput = "${gmcliArchiveRoot}/telephony";
  gmcliTelephonyFullOutput = "/home/imalison/Backups/gmcli/android-telephony-full";
  gmcliBackupLock = "/home/imalison/.local/state/gmcli/backup.lock";
  gmcliBackupLockDirectory = builtins.dirOf gmcliBackupLock;
  gmcliSession = "/home/imalison/.local/state/gmcli/session.json";
  gmcliChromeCookieDB = "/home/imalison/.config/google-chrome/Default/Cookies";
  refreshGmcliCookies = pkgs.writeShellScript "refresh-gmcli-cookies" ''
        set -euo pipefail
        umask 077
        ${gmcliCookiePython}/bin/python -c '
    import browser_cookie3
    import json
    import sys

    cookies = browser_cookie3.chrome(
        cookie_file=${builtins.toJSON gmcliChromeCookieDB},
        domain_name=".google.com",
    )
    # libgm stores cookies as a name/value map, so including cookies from
    # accounts.google.com, mail.google.com, etc. can overwrite the cookie for
    # messages.google.com and make Google redirect to CookieMismatch.
    messages_domains = {".google.com", "messages.google.com"}
    json.dump(
        {
            cookie.name: cookie.value
            for cookie in cookies
            if cookie.domain in messages_domains
        },
        sys.stdout,
    )
    ' | ${gmcliPackage}/bin/gmcli auth refresh-cookies --cookies-file -
  '';
  exportGmcliArchive = pkgs.writeShellScript "export-gmcli-archive" ''
    set -euo pipefail
    ${gmcliPackage}/bin/gmcli export jsonl --out ${lib.escapeShellArg gmcliArchiveOutput} --force
    ${gmcliPackage}/bin/gmcli export verify --dir ${lib.escapeShellArg gmcliArchiveOutput}
  '';
  exportGmcliTelephonyArchive = pkgs.writeShellScript "export-gmcli-telephony-archive" ''
    set -euo pipefail
    ${gmcliPackage}/bin/gmcli android export-telephony \
      --adb ${pkgs.android-tools}/bin/adb \
      --out ${lib.escapeShellArg gmcliTelephonyOutput} \
      --force --include-part-data=false
    ${gmcliPackage}/bin/gmcli android verify-telephony --dir ${lib.escapeShellArg gmcliTelephonyOutput}
  '';
  exportGmcliTelephonyFullArchive = pkgs.writeShellScript "export-gmcli-telephony-full-archive" ''
    set -euo pipefail
    ${gmcliPackage}/bin/gmcli android export-telephony \
      --adb ${pkgs.android-tools}/bin/adb \
      --out ${lib.escapeShellArg gmcliTelephonyFullOutput} \
      --force --include-part-data=true
    ${gmcliPackage}/bin/gmcli android verify-telephony --dir ${lib.escapeShellArg gmcliTelephonyFullOutput}
  '';
  refreshGmcliArchiveUnlocked = pkgs.writeShellScript "refresh-gmcli-archive-unlocked" ''
    set -uo pipefail
    status=0
    ${refreshGmcliCookies} || status=1
    # Never replace a healthy archive with an empty export after authentication
    # or transport failure. A successful sync is the prerequisite for export.
    if ! ${gmcliPackage}/bin/gmcli sync --include-spam=false --include-archive=false; then
      echo "gmcli sync failed; preserving the existing archive" >&2
      exit 1
    fi
    ${exportGmcliArchive} || status=1
    exit "$status"
  '';
  backfillGmcliArchiveUnlocked = pkgs.writeShellScript "backfill-gmcli-archive-unlocked" ''
    set -uo pipefail
    status=0
    ${refreshGmcliCookies} || status=1
    if ! ${gmcliPackage}/bin/gmcli sync; then
      echo "gmcli sync failed; preserving the existing archive" >&2
      exit 1
    fi
    exhausted=0
    pass=1
    while ((pass <= 20)); do
      echo "Starting gmcli deep-history pass $pass/20"
      result="$(${gmcliPackage}/bin/gmcli --json history backfill-all --requests 20 --count 100)"
      backfill_status=$?
      metrics="$(${pkgs.jq}/bin/jq -er '[.messages_added, .failed, .needs_more] | @tsv' <<<"$result")" || {
        echo "Unable to read coverage metrics from backfill result" >&2
        status=1
        break
      }
      IFS=$'\t' read -r added failed needs_more <<<"$metrics"
      echo "Deep-history pass $pass added $added message(s); $needs_more conversation(s) need more"
      if ((failed > 0)); then
        echo "Deep-history pass failed for $failed conversation(s)" >&2
        status=1
        break
      fi
      if ((backfill_status != 0 && needs_more == 0)); then
        echo "Deep-history command failed without resumable work" >&2
        status=1
        break
      fi
      if ((needs_more == 0)); then
        exhausted=1
        break
      fi
      ((pass++))
    done
    if ((status == 0 && exhausted == 0)); then
      echo "Deep-history backfill hit the 20-pass safety cap before exhaustion" >&2
      status=1
    fi
    ${exportGmcliArchive} || status=1
    ${exportGmcliTelephonyArchive} || status=1
    ${gmcliPackage}/bin/gmcli coverage verify || status=1
    exit "$status"
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
  refreshGmcliArchive = withGmcliBackupLock "refresh-gmcli-archive" refreshGmcliArchiveUnlocked;
  gmcliViewer = pkgs.symlinkJoin {
    name = "gmcli-viewer-with-managed-sync";
    paths = [gmcliViewerBase];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      wrapProgram "$out/bin/gmcli-viewer" \
        --set GMCLI_ARCHIVE_SYNC_COMMAND ${lib.escapeShellArg refreshGmcliArchive}
    '';
  };
  backfillGmcliArchive = withGmcliBackupLock "backfill-gmcli-archive" backfillGmcliArchiveUnlocked;
  backupGmcliTelephonyFull = withGmcliBackupLock "backup-gmcli-telephony-full" exportGmcliTelephonyFullArchive;
  gitSyncRepositoryCondition = pkgs.writeShellScript "git-sync-repository-ready" ''
    set -euo pipefail
    directory="''${GIT_SYNC_DIRECTORY:?GIT_SYNC_DIRECTORY is not set}"
    if ! ${pkgs.git}/bin/git -C "$directory" rev-parse --show-toplevel >/dev/null 2>&1; then
      echo "Git sync paused: $directory is not a Git repository; initialize its history repository before starting this service" >&2
      exit 1
    fi
  '';
  mkGitSyncTrayOverrides = icon: {
    Unit = {
      StartLimitIntervalSec = 300;
      StartLimitBurst = 3;
    };
    Service = {
      Environment = lib.mkMerge [
        ["GIT_SYNC_TRAY=1" "GIT_SYNC_TRAY_ICON=${icon}"]
        (lib.mkAfter ["PATH=${gitSyncServicePath}"])
      ];
      Restart = lib.mkForce "on-failure";
      RestartSec = 5;
    };
  };
  repoIcons = {
    org = "${pkgs.papirus-icon-theme}/share/icons/Papirus/64x64/mimetypes/text-org.svg";
    password-store = "password";
    # Brand logos from the desktop apps' hicolor theme icons (resolved by
    # freedesktop name, like "password" above).
    claude-history = "claude-desktop";
    codex-history = "codex-desktop";
    gmcli-archive = "mail-message-new";
  };
in {
  environment.systemPackages = [gmcliViewer];

  home-manager.users.imalison = {config, ...}: {
    services.git-sync = {
      enable = true;
      package = pkgs.git-sync-rs;
      repositories =
        {
          org = {
            path = config.home.homeDirectory + "/org";
            uri = "git@github.com:IvanMalison/org.git";
            interval = 30;
          };
          password-store = {
            path = config.home.homeDirectory + "/.password-store";
            uri = "git@github.com:IvanMalison/.password-store.git";
          };
          gmcli-archive = {
            path = gmcliArchiveRoot;
            uri = "git@github.com:colonelpanic8/gmcli-archive.git";
            interval = 300;
          };
        }
        // lib.optionalAttrs syncAiHistory {
          claude-history = {
            path = config.home.homeDirectory + "/.claude";
            uri = "git@github.com:colonelpanic8/claude-history.git";
            interval = 600;
          };
          codex-history = {
            path = config.home.homeDirectory + "/.codex";
            uri = "git@github.com:colonelpanic8/codex-history.git";
            interval = 600;
          };
        };
    };

    systemd.user.services = lib.mkMerge [
      (lib.mapAttrs'
        (name: _:
          lib.nameValuePair "git-sync-${name}"
          (mkGitSyncTrayOverrides (repoIcons.${name} or "git")))
        config.services.git-sync.repositories)
      (lib.optionalAttrs syncAiHistory {
        git-sync-claude-history.Service.ExecCondition = gitSyncRepositoryCondition;
        # Live sessions append to their transcript on every message; sync
        # untracked session files and throttle event-driven syncs so an
        # active session doesn't push once per append. Avoid making a
        # retryable initial-sync error terminate watch mode; subsequent file
        # and periodic sync attempts remain active and use git-sync's backoff.
        git-sync-claude-history.Service.ExecStart =
          lib.mkForce
          "${pkgs.git-sync-rs}/bin/git-sync-rs watch --no-initial-sync --new-files true --min-interval 300 --watch-path projects --watch-path history.jsonl --watch-path plans --watch-path tasks";
        git-sync-codex-history.Service.ExecCondition = gitSyncRepositoryCondition;
        git-sync-codex-history.Service.ExecStart =
          lib.mkForce
          "${pkgs.git-sync-rs}/bin/git-sync-rs watch --no-initial-sync --new-files true --min-interval 300 --watch-path sessions --watch-path archived_sessions --watch-path history.jsonl";
      })
      {
        git-sync-gmcli-archive.Service.ExecStart =
          lib.mkForce
          "${pkgs.git-sync-rs}/bin/git-sync-rs -d ${lib.escapeShellArg gmcliArchiveRoot} watch --new-files true --min-interval 30 --interval 300";
        gmcli-archive-refresh = {
          Unit = {
            Description = "Sync Google Messages and refresh the JSONL archive";
            ConditionPathExists = gmcliSession;
          };
          Service = {
            Type = "oneshot";
            ExecStart = refreshGmcliArchive;
            TimeoutStartSec = "20min";
          };
        };
        gmcli-archive-backfill = {
          Unit = {
            Description = "Deep-backfill Google Messages and refresh the JSONL archive";
            ConditionPathExists = gmcliSession;
          };
          Service = {
            Type = "oneshot";
            ExecStart = backfillGmcliArchive;
            TimeoutStartSec = "3h";
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
      }
    ];

    systemd.user.timers.gmcli-archive-refresh = {
      Unit.Description = "Hourly Google Messages JSONL backup";
      Timer = {
        OnCalendar = "hourly";
        Persistent = true;
        RandomizedDelaySec = "5min";
      };
      Install.WantedBy = ["timers.target"];
    };

    systemd.user.timers.gmcli-archive-backfill = {
      Unit.Description = "Daily deep Google Messages history backfill";
      Timer = {
        OnCalendar = "*-*-* 04:00:00";
        Persistent = true;
        RandomizedDelaySec = "2h";
      };
      Install.WantedBy = ["timers.target"];
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

  home-manager.users.kat = {config, ...}: {
    services.git-sync = {
      enable = true;
      repositories = {
        obsidian = {
          path = config.home.homeDirectory + "/obsidian";
          uri = "git@github.com:katandtonic/obsidian.git";
        };
        org = {
          path = config.home.homeDirectory + "/org";
          uri = "ssh://gitea@1896Folsom.duckdns.org:1123/kkathuang/org.git";
          interval = 180;
        };
      };
    };
  };
}
