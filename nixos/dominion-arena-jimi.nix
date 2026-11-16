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
  playbook = builtins.fetchGit {
    url = "https://github.com/colonelpanic8/dominion-playbook.git";
    rev = "de18791c9c8db309d84ff6cf10790537b8eba7b7";
  };
  playbookInstructions = lib.concatMapStringsSep "\n\n" (path:
    "# ${path}\n${builtins.readFile (playbook + "/${path}")}") [
    "context/rules.md"
    "context/strategy.md"
    "context/card-power.md"
    "context/card-ratings.md"
    "context/combos.md"
    "skills/kingdom-analysis/SKILL.md"
    "skills/play-turn/SKILL.md"
    "skills/play-game/SKILL.md"
  ];
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
    settings = {
      listen = "127.0.0.1";
      port = backendPort;
      public_url = "https://${domain}";
      # nginx is the only client: it names the real one in X-Forwarded-For.
      trust_proxy = true;
      agent_seats_per_day = 10;
      online_enabled = true;
      online_owner = "Arena Matches";
      online_account = "colonelpanic8";
      online_exclusive = true;
      online_resume = true;
    };
    agents = {
      enable = true;
      user = "imalison";
      settings.agents = [
        {
          name = "Sol High";
          program = "codex";
          model = "gpt-6.1-sol";
          effort = "high";
          instructions = playbookInstructions;
        }
        {
          name = "Terra";
          program = "codex";
          model = "gpt-6.1-terra";
          effort = "high";
        }
        {
          name = "Argon High";
          program = "antigravity";
          model = "gemini-4-argon-high";
          command = lib.getExe pkgs.antigravity-cli;
          auth_dir = "/home/imalison/.gemini";
        }
        {
          name = "Sonnet High";
          program = "antigravity";
          model = "claude-sonnet-5-5-high";
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
