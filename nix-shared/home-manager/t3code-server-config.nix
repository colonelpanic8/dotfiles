# Server declarations shared by every T3 Code fleet member, NixOS and macOS
# alike. Values are managed: clients show them as fixed by system configuration.
{lib, ...}: {
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
          model = "claude-fable-5-1";
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
    };
    keybindings = builtins.fromJSON (builtins.readFile ../t3code-keybindings.json);
    fleetManifest = import ../t3code-fleet.nix;
  };
}
