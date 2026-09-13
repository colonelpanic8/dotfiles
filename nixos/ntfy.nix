{
  config,
  makeEnable,
  ...
}:
makeEnable config "myModules.ntfy" false {
  services.ntfy-sh = {
    enable = true;
    settings = {
      base-url = "http://${config.networking.hostName}:2586";
      listen-http = ":2586";
    };
  };

  # Topics are unauthenticated, so expose the port only through Tailscale.
  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [2586];
}
