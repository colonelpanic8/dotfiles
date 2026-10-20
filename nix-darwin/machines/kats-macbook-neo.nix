{
  config,
  lib,
  ...
}: {
  imports = [
    ../persistent-desktop.nix
    ../auto-upgrade.nix
    (import ../tailscale.nix {
      authKeyFile = ../../nixos/secrets/tailscale-authkey.kats-macbook-neo.age;
    })
  ];

  nix.enable = false;
  nixpkgs.hostPlatform = "aarch64-darwin";
  networking.hostName = "kats-macbook-neo";
  system.primaryUser = "kat";
  system.stateVersion = 7;

  age.identityPaths = ["/etc/ssh/ssh_host_ed25519_key"];

  environment.etc."nix/nix.custom.conf".text =
    lib.concatStringsSep "\n" (lib.mapAttrsToList (
      name: value: "${
        if builtins.elem name ["substituters" "trusted-public-keys"]
        then "extra-${name}"
        else name
      } = ${
        if builtins.isList value
        then lib.concatStringsSep " " value
        else toString value
      }"
    ) (import ../nix-settings.nix))
    + "\n";
  environment.etc."nix/nix.custom.conf".knownSha256Hashes = [
    (builtins.hashString "sha256" config.environment.etc."nix/nix.custom.conf".text)
  ];

  users.users.kat = {
    name = "kat";
    home = "/Users/kat";
    openssh.authorizedKeys.keys = let
      keys = import ../../nix-shared/keys.nix;
    in
      keys.userKeys.kat ++ keys.katKeys;
  };

  programs.bash.enable = true;

  home-manager.users.kat = {
    dotfiles.gitIdentity = {
      name = "Kat Huang";
      email = "kkathuang@gmail.com";
    };
    services.t3code.fleetManifest = lib.mkForce null;
    home.sessionVariables.T3CODE_MANAGED_CONNECTIONS_FILE = "/Users/kat/.config/t3code/managed-connections.json";
    xdg.configFile."t3code/managed-connections.json".text = builtins.toJSON {
      version = 1;
      connections = [
        {
          environmentId = "fleet:kats-macbook-neo";
          label = "kats-macbook-neo";
          httpBaseUrl = "http://127.0.0.1:3774/";
          wsBaseUrl = "ws://127.0.0.1:3774/";
          token."$file" = config.age.secrets."t3code-managed-access-token.kats-macbook-neo".path;
        }
      ];
    };
  };
  launchd.user.envVariables.T3CODE_MANAGED_CONNECTIONS_FILE = "/Users/kat/.config/t3code/managed-connections.json";
}
