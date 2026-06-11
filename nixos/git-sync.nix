{
  pkgs,
  lib,
  config,
  ...
}: let
  gitSyncServicePath = lib.makeBinPath [pkgs.coreutils pkgs.git pkgs.openssh];
  # AI chat-history sync (Claude Code + Codex) is rolled out machine-by-machine;
  # each new machine needs its existing history merged into the repo first (see
  # github.com/colonelpanic8/claude-history and .../codex-history).
  aiHistoryHosts = ["ryzen-shine" "railbird-sf" "jay-lenovo" "strixi-minaj"];
  syncAiHistory = builtins.elem config.networking.hostName aiHistoryHosts;
  mkGitSyncTrayOverrides = icon: {
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
  };
in {
  home-manager.users.imalison = {config, ...}: {
    services.git-sync = {
      enable = true;
      package = pkgs.git-sync-rs;
      repositories = {
        org = {
          path = config.home.homeDirectory + "/org";
          uri = "git@github.com:IvanMalison/org.git";
          interval = 30;
        };
        password-store = {
          path = config.home.homeDirectory + "/.password-store";
          uri = "git@github.com:IvanMalison/.password-store.git";
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
        # Live sessions append to their transcript on every message; sync
        # untracked session files and throttle event-driven syncs so an
        # active session doesn't push once per append.
        git-sync-claude-history.Service.ExecStart =
          lib.mkForce
          "${pkgs.git-sync-rs}/bin/git-sync-rs watch --new-files true --min-interval 300";
        git-sync-codex-history.Service.ExecStart =
          lib.mkForce
          "${pkgs.git-sync-rs}/bin/git-sync-rs watch --new-files true --min-interval 300";
      })
    ];
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
