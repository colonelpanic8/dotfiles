{ ... }: {
  home-manager.users.imalison = ({ config, ... }: {
    services.git-sync = {
      enable = true;
      repositories = {
        config = {
          path = config.home.homeDirectory + "/config";
          uri = "git@github.com:IvanMalison/config.git";
        };
        org = {
          path = config.home.homeDirectory + "/org";
          uri = "git@github.com:IvanMalison/org.git";
        };
        password-store = {
          path = config.home.homeDirectory + "/.password-store";
          uri = "git@github.com:IvanMalison/.password-store.git";
        };
        katnivan = {
          path = config.home.homeDirectory + "/katnivan";
          uri = "ssh://gitea@1896Folsom.duckdns.org:1123/colonelpanic/katnivan.git";
        };
      };
    };
  });

  home-manager.users.kat = ({ config, ... }: {
    services.git-sync = {
      enable = true;
      repositories = {
        obsidian = {
          path = config.home.homeDirectory + "/obsidian";
          uri = "git@github.com:katandtonic/obsidian.git";
          interval = 60;
        };
        org = {
          path = config.home.homeDirectory + "/org";
          uri = "ssh://gitea@1896Folsom.duckdns.org:1123/kkathuang/org.git";
          interval = 45;
        };
        katnivan = {
          path = config.home.homeDirectory + "/katnivan";
          uri = "ssh://gitea@1896Folsom.duckdns.org:1123/colonelpanic/katnivan.git";
        };
      };
    };
  });
}
