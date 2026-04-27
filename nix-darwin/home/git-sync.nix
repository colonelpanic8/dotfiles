{
  config,
  lib,
  pkgs,
  ...
}: let
  gitSyncPackage =
    if pkgs ? "git-sync-rs"
    then pkgs."git-sync-rs"
    else pkgs.git-sync;
  orgPath = "${config.home.homeDirectory}/org";
  passwordStorePath = "${config.home.homeDirectory}/.password-store";
in {
  services.git-sync = {
    enable = true;
    package = gitSyncPackage;
    repositories = {
      org = {
        path = orgPath;
        uri = "git@github.com:colonelpanic8/org.git";
        interval = 180;
      };
      password-store = {
        path = passwordStorePath;
        uri = "git@github.com:colonelpanic8/.password-store.git";
      };
    };
  };

  # git-sync-rs does not infer the repository from launchd's WorkingDirectory.
  launchd.agents = {
    git-sync-org.config.ProgramArguments =
      lib.mkForce ["${gitSyncPackage}/bin/git-sync" "-d" orgPath];
    git-sync-password-store.config.ProgramArguments =
      lib.mkForce ["${gitSyncPackage}/bin/git-sync" "-d" passwordStorePath];
  };
}
