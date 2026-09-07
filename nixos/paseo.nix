{
  config,
  inputs,
  lib,
  makeEnable,
  pkgs,
  ...
}:
makeEnable config "myModules.paseo" false {
  imports = [inputs.paseo.nixosModules.default];

  services.paseo = {
    enable = true;
    user = config.myModules.primaryUser;
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
      preStart = let
        ensurePaseoDaemonSettings = import ../nix-shared/ensure-paseo-daemon-settings.nix {
          inherit pkgs;
          settings = {
            daemon.mcp.injectIntoAgents = true;
            daemon.agentProfiles = [
              {
                id = "legacy_favorite:claude:claude-fable-5-1";
                name = "Fable 5.1";
                provider = "claude";
                model = "claude-fable-5-1";
              }
              {
                id = "legacy_favorite:codex:gpt-6-astra";
                name = "GPT-6-Astra";
                provider = "codex";
                model = "gpt-6-astra";
              }
              {
                id = "legacy_favorite:codex:gpt-5.6-sol";
                name = "GPT-5.6-Sol";
                provider = "codex";
                model = "gpt-5.6-sol";
              }
              {
                id = "legacy_favorite:codex:gpt-5.6-luna";
                name = "GPT-5.6-Luna";
                provider = "codex";
                model = "gpt-5.6-luna";
              }
              {
                id = "legacy_favorite:claude:claude-opus-5";
                name = "Opus 5";
                provider = "claude";
                model = "claude-opus-5";
              }
            ];

            # Live Voice reads these files fresh at the start of every call and
            # injects them as context, so the voice chief of staff knows how the
            # org repo is laid out and what is currently going on without being
            # told each time. Keep the list short: each file is capped at ~12KiB
            # and the set at ~32KiB, and everything here competes with the agent
            # and workspace snapshots for the same startup budget. Live task
            # state is not here on purpose -- gtd.org alone is 100KiB+ and would
            # be truncated. Ask the agenda through a tool or a delegated session
            # instead.
            liveVoice = {
              defaultContextProfile = "life";
              contextProfiles = [
                {
                  id = "life";
                  label = "Life";
                  files = [
                    "~/org/AGENTS.md"
                    "~/org/agents/profile.org"
                    "~/org/planning/context.org"
                  ];
                  instructions = ''
                    You are Ivan's chief of staff for life logistics as well as
                    code. The files above describe how his org-mode GTD system
                    is organized, how he works, and what is currently going on.
                    Treat them as background, not as a script to read back.

                    Anything he mentions wanting to do, remember, or follow up
                    on is a capture: route it into his inbox rather than holding
                    it in the conversation. Route real work to sessions that can
                    reach the files -- you are in a plain directory and should
                    not try to read or edit org files yourself.
                  '';
                }
                {
                  id = "work";
                  label = "Work";
                  files = [];
                  instructions = ''
                    Keep this call on the technical work being discussed. Do not
                    bring up personal tasks, agenda items, or life logistics
                    unless Ivan raises them.
                  '';
                }
              ];
            };
          };
        };
      in
        lib.mkAfter ''
          ${ensurePaseoDaemonSettings} ${lib.escapeShellArg "${config.services.paseo.dataDir}/config.json"}
        '';
    }
    (lib.mkIf config.myModules.tailscale.enable {
      after = ["agenix.service"];
      serviceConfig.EnvironmentFile = config.age.secrets.paseo-password-environment.path;
    })
  ];

  home-manager.users.${config.myModules.primaryUser}.imports = [
    ../nix-shared/home-manager/paseo-settings-seed.nix
  ];
}
