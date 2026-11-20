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

  age.secrets.dominion-online-astra-password.file = ./secrets/dominion-online-astra-password.age;

  services.dominion = {
    enable = true;
    syncAssets = true;
    onlinePasswordCredential = "/var/lib/private/dominion-online-password.cred";
    onlineAccounts.astra = {
      account = "AstraDominion";
      owner = "colonelpanic8";
      exclusive = true;
      passwordFile = config.age.secrets.dominion-online-astra-password.path;
      settings = {
        resume = true;
        min_opponent_level = 46;
      };
    };
    onlineAccounts.sol = {
      account = "SolDominion";
      owner = "colonelpanic8";
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
      # No forfeit for a slow decision: an agent whose provider is down waits instead.
      decision_minutes = 0;
      keep_finished_days = 0;
      registration = false;
      # They make invite codes in the Account tab; a code registers someone despite registration = false.
      admins = ["colonelpanic8"];
      guests = false;
      online_enabled = true;
      online_owner = "colonelpanic8";
      online_account = "colonelpanic8";
      online_exclusive = true;
      online_resume = true;
    };
    agents = {
      enable = true;
      user = "imalison";
      atOnce = 40;
      claude = pkgs.claude-code;
      codex = pkgs.codex;
      antigravity = pkgs.antigravity-cli;
      # The Ultra plan's monthly API credits, in Railbird's Console organization.
      anthropic = {
        apiKeyCredential = "/var/lib/private/dominion-anthropic-api-key.cred";
        workspace = "wrkspc_015TZphZF8bVV6GtDbuHUzpK";
      };
      # Antigravity seats sign in from their own copy of the token: `agy` run on the host
      # moves ~/.gemini's token file into the keyring, which would sign them all out.
      # Their conversations go in ~/.gemini all the same, where T3 Code counts usage.
      profiles = map (p:
        if p.program == "antigravity"
        then
          p
          // {
            auth_dir = "/home/imalison/.local/state/dominion/antigravity-auth";
            conversations_dir = "/home/imalison/.gemini/antigravity-cli/conversations/dominion-arena";
          }
        else p) (
        import "${inputs.dominion-arena}/nix/agents.nix"
        ++ [
          {
            name = "Opus API High";
            program = "anthropic";
            model = "claude-opus-5-5";
            effort = "high";
          }
          {
            name = "Fable API High";
            program = "anthropic";
            model = "claude-fable-5-2";
            effort = "high";
          }
          {
            name = "Hydrogen High";
            program = "antigravity";
            model = "gemini-4-hydrogen-high";
          }
          {
            name = "Neon High";
            program = "antigravity";
            model = "gemini-4-neon-high";
          }
        ]
      );
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
