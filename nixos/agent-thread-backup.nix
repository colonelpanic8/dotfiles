{
  config,
  lib,
  makeEnable,
  pkgs,
  ...
}: let
  fleet = import ../nix-shared/agent-fleet-hosts.nix;
  agentThreadBackup = pkgs.writeShellApplication {
    name = "agent-thread-backup";
    runtimeInputs = with pkgs; [bash coreutils findutils openssh python3 rsync zstd];
    text = builtins.readFile ./agent-thread-backup.sh;
  };
  hostArgs = map (name: "${name}=${fleet.sshTargets.${name} or name}") fleet.hosts;
in
  makeEnable config "myModules.agentThreadBackup" false {
    home-manager.users.${config.myModules.primaryUser} = {
      systemd.user.services.agent-thread-backup = {
        Unit.Description = "Back up agent threads from every fleet host";
        Service = {
          Type = "oneshot";
          ExecStart = "${lib.getExe agentThreadBackup} ${lib.escapeShellArgs hostArgs}";
          TimeoutStartSec = "2h";
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
