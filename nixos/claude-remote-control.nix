{
  pkgs,
  config,
  lib,
  makeEnable,
  ...
}: let
  # Directories that each get an always-on Remote Control session, so new
  # conversations can be started in any of them from claude.ai/code or the
  # Claude mobile app (without having to run `claude rc` by hand first).
  #
  # Each directory gets its own systemd user service, tmux server (dedicated
  # `-L` socket) and Remote Control session name derived from the directory's
  # base name. Attach locally with the generated `claude-rc-attach-<name>`
  # alias, e.g. `claude-rc-attach-dotfiles`.
  # The same list is used on every host that enables this module. A host that
  # doesn't have a given directory simply skips that one session at runtime via
  # `ConditionPathIsDirectory` (see below) — so we must NOT filter the list at
  # eval time. In particular `builtins.pathExists` is useless here: the
  # auto-upgrade builds this flake in *pure* evaluation mode, where pathExists
  # returns false for every absolute path and would silently drop every session.
  directories = [
    "/srv/dotfiles"
    "/home/imalison/Projects/subtr-actor"
    "/home/imalison/Projects/rocket-sense"
    "/home/imalison/code/mova"
  ];

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

  mkName = dir: baseNameOf dir;

  mkService = dir: let
    name = mkName dir;
    # Dedicated tmux socket + session name so each service owns its own tmux
    # server, independent of the interactive one and of every other session.
    socket = "claude-rc-${name}";
    sessionName = "claude-rc-${name}";
    # Name the session registers under for native Remote Control (phone/web).
    remoteName = "${config.networking.hostName}-${name}";
  in {
    name = "claude-remote-control-${name}";
    value = {
      Unit = {
        Description = "Claude Code remote-control session (${dir})";
        After = ["network.target"];
        # Skip this session cleanly (not "failed", no restart loop) on hosts
        # where the directory doesn't exist. Evaluated at runtime, so it stays
        # correct regardless of pure vs impure Nix evaluation.
        ConditionPathIsDirectory = dir;
      };
      Service = {
        # tmux new-session -d daemonizes the server and returns.
        Type = "forking";
        Environment = ["PATH=${servicePath}"];
        ExecStart = lib.concatStringsSep " " [
          "${pkgs.tmux}/bin/tmux -L ${socket} new-session -d"
          "-s ${sessionName} -c ${dir}"
          "${pkgs.claude-code}/bin/claude --remote-control ${remoteName} --dangerously-skip-permissions"
        ];
        ExecStop = "${pkgs.tmux}/bin/tmux -L ${socket} kill-server";
        Restart = "on-failure";
        RestartSec = 5;
      };
      Install.WantedBy = ["default.target"];
    };
  };

  mkAlias = dir: let
    name = mkName dir;
  in {
    name = "claude-rc-attach-${name}";
    value = "tmux -L claude-rc-${name} attach -t claude-rc-${name}";
  };
in
  makeEnable config "myModules.claudeRemoteControl" false {
    home-manager.users.imalison = {
      systemd.user.services = builtins.listToAttrs (map mkService directories);

      # Convenience: attach to any always-on session from any directory.
      home.shellAliases = builtins.listToAttrs (map mkAlias directories);
    };
  }
