# Defaults and managed provider settings shared by NixOS and macOS fleet members.
{
  lib,
  pkgs,
  ...
}: {
  services.t3code = {
    sharedSettings = builtins.fromJSON (builtins.readFile ../t3code-settings/defaults.json);
    settings = {
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
    };
    keybindings = builtins.fromJSON (builtins.readFile ../t3code-keybindings.json);
    fleetManifest = import ../t3code-fleet.nix;
  };
}
