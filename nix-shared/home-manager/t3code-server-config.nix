# Server declarations shared by every T3 Code fleet member, NixOS and macOS
# alike. Values are managed: clients show them as fixed by system configuration.
{
  lib,
  pkgs,
  ...
}: {
  services.t3code = {
    settings = {
      favorites = [
        {
          provider = "codex";
          model = "gpt-6.1-sol";
        }
        {
          provider = "codex";
          model = "gpt-6-astra";
        }
        {
          provider = "claudeAgent";
          model = "claude-fable-5-2";
        }
        {
          provider = "claudeAgent";
          model = "claude-opus-5-5";
        }
      ];
      providerInstances.claudeAgent = lib.mkDefault {
        driver = "claudeAgent";
        config.launchArgs = "--chrome";
      };
      # Gemini, now served through Antigravity. Pinning the runtime keeps T3
      # from downloading its own ~1 GB copy into ~/.t3/tools on every host.
      providerInstances.antigravity = lib.mkIf (lib.meta.availableOn pkgs.stdenv.hostPlatform pkgs.antigravity-acp) {
        driver = "antigravity";
        displayName = "Antigravity";
        enabled = true;
        config.binaryPath = "${pkgs.antigravity-acp}/bin/agy_acp_server.par";
      };
      textGenerationModelSelection = lib.mkDefault {
        instanceId = "claudeAgent";
        model = "claude-haiku-5-5";
        options = [
          {
            id = "effort";
            value = "low";
          }
        ];
      };
      hiddenPullRequestRepositories = lib.mkDefault ["pingdotgg/t3code"];
      # Both ends of a link need it, so every fleet member accepts links.
      linkedEnvironmentsEnabled = true;
    };
    keybindings = builtins.fromJSON (builtins.readFile ../t3code-keybindings.json);
    fleetManifest = import ../t3code-fleet.nix;
  };
}
