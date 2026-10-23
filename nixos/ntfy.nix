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

  # Topics are unauthenticated, so only accept connections over Tailscale.
  systemd.services.ntfy-sh.serviceConfig.RestrictNetworkInterfaces = "lo tailscale0";
}
