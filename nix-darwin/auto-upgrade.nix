{
  config,
  lib,
  pkgs,
  ...
}: let
  user = config.system.primaryUser;
  home = config.users.users.${user}.home;
  upgrade = pkgs.writeShellScript "dotfiles-auto-upgrade" ''
    set -euo pipefail
    export PATH=${lib.makeBinPath [pkgs.git pkgs.just]}:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/bin:/bin:/usr/sbin:/sbin
    cd /Users/Shared/dotfiles

    if [ "$(git branch --show-current)" != master ]; then
      echo "Skipping upgrade: dotfiles is not on master"
      exit 0
    fi
    if [ -n "$(git status --porcelain)" ]; then
      echo "Skipping upgrade: dotfiles has local changes"
      exit 0
    fi

    git pull --ff-only origin master
    cd nix-darwin
    exec just switch
  '';
in {
  launchd.daemons.dotfiles-auto-upgrade.serviceConfig = {
    ProgramArguments = ["${upgrade}"];
    UserName = user;
    GroupName = "staff";
    WorkingDirectory = "/Users/Shared/dotfiles/nix-darwin";
    EnvironmentVariables = {
      HOME = home;
      USER = user;
      LOGNAME = user;
    };
    StartCalendarInterval = [
      {
        Hour = 4;
        Minute = 30;
      }
    ];
    ProcessType = "Background";
    StandardOutPath = "${home}/Library/Logs/dotfiles-auto-upgrade.log";
    StandardErrorPath = "${home}/Library/Logs/dotfiles-auto-upgrade.err.log";
  };
}
