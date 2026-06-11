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
  claudePath = "${config.home.homeDirectory}/.claude";
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
      claude-history = {
        path = claudePath;
        uri = "git@github.com:colonelpanic8/claude-history.git";
        interval = 600;
      };
      # NB: codex-history is intentionally NOT synced on mac-demarco-mini.
      # The codex archive is ~1GB and this machine runs chronically near full
      # (APFS container ~94% used); cloning it would break every darwin
      # rebuild. mac's own Codex sessions are already merged into the repo —
      # it just doesn't receive. Re-enable once the disk has headroom.
    };
  };

  # git-sync-rs does not infer the repository from launchd's WorkingDirectory.
  launchd.agents = {
    git-sync-org.config.ProgramArguments =
      lib.mkForce ["${gitSyncPackage}/bin/git-sync" "-d" orgPath];
    git-sync-password-store.config.ProgramArguments =
      lib.mkForce ["${gitSyncPackage}/bin/git-sync" "-d" passwordStorePath];
    # Live Claude sessions append to their transcript constantly; sync
    # untracked session files and throttle event-driven syncs.
    git-sync-claude-history.config.ProgramArguments =
      lib.mkForce ["${gitSyncPackage}/bin/git-sync-rs" "-d" claudePath "watch" "--new-files" "true" "--min-interval" "300"];
  };
}
