{
  config,
  inputs,
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
}
