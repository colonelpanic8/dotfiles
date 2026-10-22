{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.familyInstaller.provisioning;
  user = config.myModules.primaryUser;
in {
  options.familyInstaller.provisioning = {
    enable = lib.mkEnableOption "pre-enrolled family machine";
    authorizedKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
    };
    tailscale = lib.mkEnableOption "pre-authorized Tailscale enrollment";
  };
  config = lib.mkIf cfg.enable {
    familyInstaller.agentTools.enable = true;
    services.openssh = {
      enable = true;
      openFirewall = false;
      settings.PasswordAuthentication = false;
      settings.KbdInteractiveAuthentication = false;
      settings.PermitRootLogin = "no";
    };
    networking.firewall.interfaces.tailscale0.allowedTCPPorts = [22];
    users.users.${user}.openssh.authorizedKeys.keys = cfg.authorizedKeys;
    services.tailscale = {
      enable = true;
      authKeyFile = lib.mkIf cfg.tailscale "/run/family-installer/tailscale-authkey";
    };
    systemd.services.family-provision = {
      wantedBy = ["multi-user.target"];
      before = ["display-manager.service" "tailscaled-autoconnect.service"];
      after = ["local-fs.target"];
      path = [pkgs.age pkgs.openssh];
      serviceConfig.Type = "oneshot";
      serviceConfig.UMask = "0077";
      script = ''
        ${pkgs.python3}/bin/python3 ${../scripts}/provisioning.py boot ${lib.escapeShellArg user}
      '';
    };
    systemd.services.tailscaled-autoconnect = lib.mkIf cfg.tailscale {
      requires = ["family-provision.service"];
      after = ["family-provision.service"];
      postStart = ''
        if [ "$(${pkgs.tailscale}/bin/tailscale status --json | ${pkgs.jq}/bin/jq -r .BackendState)" = Running ]; then
          rm -f /var/lib/family-installer/tailscale.age /run/family-installer/tailscale-authkey
        fi
      '';
    };
  };
}
