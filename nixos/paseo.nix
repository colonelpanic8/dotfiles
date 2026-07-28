{
  config,
  inputs,
  lib,
  makeEnable,
  ...
}:
makeEnable config "myModules.paseo" false {
  imports = [inputs.paseo.nixosModules.default];

  services.paseo = {
    enable = true;
    user = "imalison";
    group = "users";
    listenAddress = "0.0.0.0";
    port = 6767;

    # Accept the machine's MagicDNS short name in addition to IP addresses,
    # which Paseo permits automatically.
    hostnames = [config.networking.hostName];
  };

  # Paseo binds all addresses so it can accept the Tailscale interface, but
  # only expose its port through that interface. In particular, do not use
  # services.paseo.openFirewall, which would also expose it on LAN interfaces.
  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [
    config.services.paseo.port
  ];

  age.secrets.paseo-password-environment = lib.mkIf config.myModules.tailscale.enable {
    file = ./secrets/paseo-password-environment.age;
    owner = config.services.paseo.user;
    group = config.services.paseo.group;
    mode = "0400";
  };

  systemd.services.paseo = lib.mkMerge [
    {
      # Rebuilds driven from a terminal or agent that lives inside
      # paseo.service's own cgroup die when switch-to-configuration stops the
      # unit, so the switch's start phase never runs and paseo stays down.
      # Upholds= makes systemd itself start the unit again whenever it is
      # found inactive while multi-user.target is up.
      upheldBy = ["multi-user.target"];
    }
    (lib.mkIf config.myModules.tailscale.enable {
      after = ["agenix.service"];
      serviceConfig.EnvironmentFile = config.age.secrets.paseo-password-environment.path;
    })
  ];

  home-manager.users.imalison.imports = [
    ../nix-shared/home-manager/paseo-desktop-settings.nix
  ];
}
