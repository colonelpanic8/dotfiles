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
  gitSyncConfig = (pkgs.formats.toml {}).generate "git-sync-rs-config.toml" {
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
        path = "${config.home.homeDirectory}/org";
        uri = "git@github.com:colonelpanic8/org.git";
        watch = true;
        interval = 180;
      }
      {
        name = "password-store";
        path = "${config.home.homeDirectory}/.password-store";
        uri = "git@github.com:colonelpanic8/.password-store.git";
        watch = true;
      }
      {
        name = "claude-history";
        path = "${config.home.homeDirectory}/.claude";
        uri = "git@github.com:colonelpanic8/claude-history.git";
        watch = true;
        interval = 600;
        min_interval = 300.0;
        watch_paths = ["projects" "history.jsonl" "plans" "tasks"];
      }
      # NB: codex-history is intentionally NOT synced on mac-demarco-mini.
      # The codex archive is ~1GB and this machine runs chronically near full
      # (APFS container ~94% used); cloning it would break every darwin
      # rebuild. mac's own Codex sessions are already merged into the repo —
      # it just doesn't receive. Re-enable once the disk has headroom.
    ];
  };
in {
  launchd.agents.git-sync-rs = {
    enable = true;
    config = {
      EnvironmentVariables = {
        PATH = lib.makeBinPath [pkgs.coreutils pkgs.git pkgs.openssh];
      };
      ProgramArguments = [
        "${gitSyncPackage}/bin/git-sync-rs"
        "--config"
        (toString gitSyncConfig)
        "watch"
      ];
      KeepAlive = {
        Crashed = false;
        SuccessfulExit = false;
      };
      ProcessType = "Background";
      RunAtLoad = true;
      StandardOutPath = "${config.home.homeDirectory}/Library/Logs/git-sync-rs.log";
      StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/git-sync-rs.err.log";
    };
  };
}
