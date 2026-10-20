{
  config,
  lib,
  pkgs,
  ...
}: {
  networking.hostName = "alexanders-macbook-air";
  system.stateVersion = 7;
  environment.systemPackages = [pkgs.bitwarden-desktop];
  age.identityPaths = ["/etc/ssh/ssh_host_ed25519_key"];

  users.users.alex = {
    name = "alex";
    home = "/Users/alex";
    openssh.authorizedKeys.keys = (import ../../nix-shared/keys.nix).userKeys.alex;
  };

  home-manager.users.alex = {
    services.t3code = {
      fleetManifest = lib.mkForce null;
      tailscaleServe.enable = false;
    };
    home.sessionVariables.T3CODE_MANAGED_CONNECTIONS_FILE = "/Users/alex/.config/t3code/managed-connections.json";
    xdg.configFile."t3code/managed-connections.json".text = builtins.toJSON {
      version = 1;
      connections = [
        {
          environmentId = "fleet:alexanders-macbook-air";
          label = "alexanders-macbook-air";
          httpBaseUrl = "http://127.0.0.1:3774/";
          wsBaseUrl = "ws://127.0.0.1:3774/";
          token."$file" = config.age.secrets."t3code-managed-access-token.alexanders-macbook-air".path;
        }
      ];
    };
  };
  launchd.user.envVariables.T3CODE_MANAGED_CONNECTIONS_FILE = "/Users/alex/.config/t3code/managed-connections.json";
}
