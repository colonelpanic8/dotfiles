{
  config,
  lib,
  makeEnable,
  pkgs,
  ...
}: let
  fleet = import ../nix-shared/agent-fleet-hosts.nix;
  agentThreadsIndex = pkgs.writeShellApplication {
    name = "agent-threads-index";
    runtimeInputs = with pkgs; [openssh python3 zstd];
    text = ''exec python3 -I ${./agent-threads/index.py} "$@"'';
  };
  agentThreadBackup = pkgs.writeShellApplication {
    name = "agent-thread-backup";
    runtimeInputs = with pkgs; [agentThreadsIndex bash coreutils findutils openssh python3 rsync zstd];
    text = builtins.readFile ./agent-threads/backup.sh;
  };
  hostArgs = map (name: "${name}=${fleet.sshTargets.${name} or name}") fleet.hosts;
in
  makeEnable config "myModules.agentThreadBackup" false {
    home-manager.users.${config.myModules.primaryUser} = {
      home.packages = [agentThreadsIndex pkgs.sqlite-interactive];
      systemd.user.services.agent-thread-backup = {
        Unit.Description = "Back up and index agent threads from every fleet host";
        Service = {
          Type = "oneshot";
          ExecStart = "${lib.getExe agentThreadBackup} ${lib.escapeShellArgs hostArgs}";
          TimeoutStartSec = "3h";
        };
      };
      systemd.user.timers.agent-thread-backup = {
        Unit.Description = "Daily agent thread backup";
        Timer = {
          OnCalendar = "daily";
          Persistent = true;
          RandomizedDelaySec = "1h";
        };
        Install.WantedBy = ["timers.target"];
      };
    };
  }
