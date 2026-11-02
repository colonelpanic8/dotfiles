{
  config,
  lib,
  pkgs,
  ...
}: let
  gitSyncServicePath = lib.makeBinPath [pkgs.coreutils pkgs.git pkgs.openssh];
  gitSyncToml = pkgs.formats.toml {};
in {
  config = lib.mkIf config.myUsers.kat.has.full {
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
  };
}
