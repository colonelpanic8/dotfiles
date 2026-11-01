{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myModules.tailscale;
  enroll = cfg.enrollment != null;
in {
  imports = [../nix-shared/tailscale];

  config = lib.mkIf cfg.enable {
    services.tailscale.enable = true;
    environment.systemPackages = [pkgs.tailscale];

    assertions = lib.optional enroll {
      assertion = config.services.tailscale.authKeyFile == null;
      message = "Do not combine a Tailscale enrollment profile with services.tailscale.authKeyFile.";
    };

    # Preserve existing node state and daemon preferences during adoption.
    systemd.services.tailscale-autoconnect = lib.mkIf enroll {
      description = "Enroll Tailscale using the machine's selected profile";
      wantedBy = ["multi-user.target"];
      wants = ["network-online.target" "tailscaled.service"];
      after = ["network-online.target" "tailscaled.service"];
      restartIfChanged = false;
      stopIfChanged = false;
      environment = cfg.enrollmentEnvironment;
      serviceConfig = {
        Type = "oneshot";
        User = "root";
        UMask = "0077";
        TimeoutStartSec = "45s";
        ExecStart = "${pkgs.bash}/bin/bash ${../nix-shared/tailscale/enroll.sh}";
      };
    };
    systemd.timers.tailscale-autoconnect = lib.mkIf enroll {
      description = "Retry Tailscale enrollment using the selected profile";
      wantedBy = ["timers.target"];
      timerConfig = {
        OnBootSec = "2m";
        OnUnitActiveSec = "30m";
      };
    };
  };
}
