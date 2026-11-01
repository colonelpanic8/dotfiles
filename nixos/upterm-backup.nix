{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myModules.uptermBackup;
  account = config.users.users.${cfg.user};
  authorizedKeys = pkgs.writeText "upterm-backup-authorized-keys" (
    lib.concatStringsSep "\n" cfg.authorizedKeys + "\n"
  );
in {
  options.myModules.uptermBackup = {
    enable = lib.mkEnableOption "key-restricted backup terminal access over Upterm";
    user = lib.mkOption {
      type = lib.types.str;
      default = config.myModules.primaryUser;
    };
    authorizedKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = account.openssh.authorizedKeys.keys;
      description = "Public keys permitted to access the backup terminal and SSH forwarding.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.authorizedKeys != [] && lib.all (key: key != "") cfg.authorizedKeys;
        message = "Upterm backup access requires an explicit nonempty authorized-key list.";
      }
    ];
    environment.systemPackages = [pkgs.upterm];
    environment.etc."upterm-backup/known_hosts".source = ./assets/upterm-known-hosts;

    # A system service starts before desktop login and uses only the public
    # Internet. It has no dependency on tailscaled or the tailnet's identity.
    systemd.services.upterm-backup = {
      description = "Alex's key-restricted backup terminal via the public Upterm relay";
      wantedBy = ["multi-user.target"];
      wants = ["network-online.target"];
      after = ["network-online.target"];
      startLimitIntervalSec = 0;
      path = [pkgs.coreutils pkgs.openssh];
      environment = {
        HOME = account.home;
        XDG_RUNTIME_DIR = "/run/upterm-backup";
        XDG_STATE_HOME = "/var/lib/upterm-backup";
        TERM = "xterm-256color";
      };
      preStart = ''
        test -s ${authorizedKeys}
        ${pkgs.openssh}/bin/ssh-keygen -lf ${authorizedKeys} >/dev/null
        if [ ! -e /var/lib/upterm-backup/id_ed25519 ]; then
          ${pkgs.openssh}/bin/ssh-keygen -q -t ed25519 -N "" \
            -C upterm-backup@${config.networking.hostName} \
            -f /var/lib/upterm-backup/id_ed25519
        fi
      '';
      serviceConfig = {
        User = cfg.user;
        Group = account.group;
        WorkingDirectory = account.home;
        RuntimeDirectory = "upterm-backup";
        RuntimeDirectoryMode = "0700";
        StateDirectory = "upterm-backup";
        StateDirectoryMode = "0700";
        UMask = "0077";
        Restart = "always";
        RestartSec = "10s";
        TimeoutStopSec = "15s";
        ExecStart = lib.escapeShellArgs [
          "${pkgs.upterm}/bin/upterm"
          "host"
          "--accept"
          "--server"
          "wss://uptermd.upterm.dev:443"
          "--authorized-keys"
          authorizedKeys
          "--known-hosts"
          "/etc/upterm-backup/known_hosts"
          "--private-key"
          "/var/lib/upterm-backup/id_ed25519"
          "--allow-local-tcp-forwarding"
          "--force-command"
          "${lib.getExe account.shell} -l"
          "--"
          "${pkgs.coreutils}/bin/sleep"
          "infinity"
        ];
      };
    };
  };
}
