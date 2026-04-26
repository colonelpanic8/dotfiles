{
  config,
  pkgs,
  ...
}: {
  services.git-sync = {
    enable = true;
    package =
      if pkgs ? "git-sync-rs"
      then pkgs."git-sync-rs"
      else pkgs.git-sync;
    repositories = {
      org = {
        path = "${config.home.homeDirectory}/org";
        uri = "git@github.com:colonelpanic8/org.git";
        interval = 180;
      };
      password-store = {
        path = "${config.home.homeDirectory}/.password-store";
        uri = "git@github.com:IvanMalison/.password-store.git";
      };
    };
  };
}
