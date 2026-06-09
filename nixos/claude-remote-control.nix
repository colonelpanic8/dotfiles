{
  pkgs,
  config,
  lib,
  makeEnable,
  ...
}: let
  # Working directory the always-on session is pinned to. Ad-hoc sessions in
  # other directories are handled by the `tmclaude` shell function instead.
  workingDirectory = "/srv/dotfiles";

  # Dedicated tmux socket + session name so the service owns its own tmux
  # server (independent of the interactive one). Attach locally with:
  #   tmux -L claude-rc attach -t claude-rc
  socket = "claude-rc";
  sessionName = "claude-rc";

  # Name the session registers under for native Remote Control (phone/web).
  remoteName = config.networking.hostName;

  # claude shells out to these for its tools; give the service a clean PATH.
  servicePath = lib.makeBinPath (with pkgs; [
    claude-code
    tmux
    bashInteractive
    coreutils
    findutils
    git
    gnugrep
    gnused
    nix
    nodejs
    openssh
    ripgrep
    zsh
  ]);
in
  makeEnable config "myModules.claudeRemoteControl" false {
    home-manager.users.imalison = {
      systemd.user.services.claude-remote-control = {
        Unit = {
          Description = "Claude Code remote-control session";
          After = ["network.target"];
        };
        Service = {
          # tmux new-session -d daemonizes the server and returns.
          Type = "forking";
          Environment = ["PATH=${servicePath}"];
          ExecStart = lib.concatStringsSep " " [
            "${pkgs.tmux}/bin/tmux -L ${socket} new-session -d"
            "-s ${sessionName} -c ${workingDirectory}"
            "${pkgs.claude-code}/bin/claude --remote-control ${remoteName} --dangerously-skip-permissions"
          ];
          ExecStop = "${pkgs.tmux}/bin/tmux -L ${socket} kill-server";
          Restart = "on-failure";
          RestartSec = 5;
        };
        Install.WantedBy = ["default.target"];
      };

      # Convenience: attach to the always-on session from any directory.
      home.shellAliases.claude-rc-attach = "tmux -L ${socket} attach -t ${sessionName}";
    };
  }
