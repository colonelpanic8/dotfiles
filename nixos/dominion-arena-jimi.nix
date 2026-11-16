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
    settings = {
      listen = "127.0.0.1";
      port = backendPort;
      public_url = "https://${domain}";
      # nginx is the only client: it names the real one in X-Forwarded-For.
      trust_proxy = true;
    };
    agents = {
      enable = true;
      user = "imalison";
      settings.agents = [
        {
          name = "Claude Opus";
          program = "claude";
          model = "opus";
        }
        {
          name = "Claude Sonnet";
          program = "claude";
          model = "sonnet";
        }
        {
          name = "Codex";
          program = "codex";
        }
        {
          name = "Sol 6.1";
          program = "codex";
          model = "gpt-6.1-sol";
          effort = "high";
        }
        {
          name = "Sonnet via Gemini";
          program = "antigravity";
          model = "claude-sonnet-5-5-high";
          instructions = "Game actions are JSON reply formats, not native function calls. Never invoke a native tool, including finish. Return exactly one game-action JSON object as plain assistant text.";
          command = lib.getExe pkgs.antigravity-cli;
          auth_dir = "/home/imalison/.gemini";
        }
        {
          name = "Gemini Hydrogen";
          program = "antigravity";
          model = "gemini-4-hydrogen-high";
          command = lib.getExe pkgs.antigravity-cli;
          auth_dir = "/home/imalison/.gemini";
        }
      ];
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
