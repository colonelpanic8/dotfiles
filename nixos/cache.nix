{
  config,
  lib,
  ...
}: let
  # Peers that enable myModules.cache-server (keep in sync with machines/*.nix).
  # nixquick also runs one but has been off the tailnet for months; re-add it
  # here and in the justfile when it comes back.
  cacheHosts = [
    {
      name = "ryzen-shine";
      port = 3090;
    }
    {
      name = "strixi-minaj";
      port = 3090;
    }
  ];
  peers = builtins.filter (h: h.name != config.networking.hostName) cacheHosts;
in {
  nix.settings = {
    # Bare hostnames resolve over tailscale MagicDNS (and LAN DNS when home).
    extra-substituters = map (h: "http://${h.name}:${toString h.port}") peers;
    extra-trusted-public-keys = [(lib.fileContents ./secrets/cache-pub-key.pem)];
    # Skip unreachable peer caches after a few seconds instead of hanging.
    # Nix retries failed downloads with backoff, so a dead peer costs
    # download-attempts * connect-timeout once per invocation.
    connect-timeout = 3;
    download-attempts = 2;
    # If substitution fails (peer offline mid-download), build locally.
    fallback = true;
  };
}
