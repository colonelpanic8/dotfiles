{ pkgs, lib, ... }:
let
  mkGitSyncTrayOverrides = icon: {
    Service = {
      Environment = [ "GIT_SYNC_TRAY=1" "GIT_SYNC_TRAY_ICON=${icon}" ];
      Restart = lib.mkForce "on-failure";
      RestartSec = 5;
    };
  };
  repoIcons = {
    org = "text-org";
    password-store = "password";
  };
in {
  home-manager.users.imalison = ({ config, ... }: {
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
      };
    };

    systemd.user.services = lib.mapAttrs'
      (name: _: lib.nameValuePair "git-sync-${name}"
        (mkGitSyncTrayOverrides (repoIcons.${name} or "git")))
      config.services.git-sync.repositories;
  });

  home-manager.users.kat = ({ config, ... }: {
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
  });
}
