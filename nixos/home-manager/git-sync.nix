{ config, ... }: {
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
    };
  };
}
