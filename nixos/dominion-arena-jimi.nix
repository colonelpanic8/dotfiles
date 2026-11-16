{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  domain = "dominion-arena.duckdns.org";
  lanAddress = "192.168.0.214";
  backendPort = 47317;
  # Where rocket-sense-jimi.nix's SNI map sends this name.
  tlsPort = 45445;
in {
  imports = [inputs.dominion-arena.nixosModules.default];

  assertions = [
    {
      assertion = config.networking.hostName == "jimi-hendnix";
      message = "dominion-arena-jimi.nix must only be imported on jimi-hendnix";
    }
  ];

  services.dominion = {
    enable = true;
    syncAssets = true;
    onlinePasswordCredential = "/var/lib/private/dominion-online-password.cred";
    onlineAccounts.sol = {
      account = "SolDominion";
      owner = "Arena Matches";
      exclusive = true;
      encryptedCredential = "/var/lib/private/dominion-online-sol-password.cred";
      settings.resume = true;
    };
    settings = {
      listen = "127.0.0.1";
      port = backendPort;
      public_url = "https://${domain}";
      # nginx is the only client: it names the real one in X-Forwarded-For.
      trust_proxy = true;
      agent_seats_per_day = 0;
      keep_finished_days = 0;
      registration = false;
      guests = false;
      online_enabled = true;
      online_owner = "Arena Matches";
      online_account = "colonelpanic8";
      online_exclusive = true;
      online_resume = true;
    };
    agents = {
      enable = true;
      user = "imalison";
      atOnce = 4;
      claude = pkgs.claude-code;
      codex = pkgs.codex;
      antigravity = pkgs.antigravity-cli;
    };
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
    listen = [
      {
        addr = lanAddress;
        port = 80;
      }
      {
        addr = "127.0.0.1";
        port = tlsPort;
        ssl = true;
        extraParameters = ["proxy_protocol"];
      }
    ];
    extraConfig = ''
      set_real_ip_from 127.0.0.1;
      real_ip_header proxy_protocol;
    '';
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString backendPort}";
      # Event streams and MCP sessions stay open and are sent as they come.
      extraConfig = ''
        proxy_buffering off;
        proxy_read_timeout 1h;
      '';
    };
  };
}
