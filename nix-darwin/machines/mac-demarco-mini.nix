{
  config,
  lib,
  pkgs,
  ...
}: let
  primaryUser = config.system.primaryUser;
  personalUsers = ["kat" "imalison"];
  homeForUser = user: "/Users/${user}";
  mkUserDescription = user:
    if user == "imalison"
    then "Ivan Malison"
    else null;
in {
  networking.hostName = "mac-demarco-mini";
  imports = [
    ../persistent-desktop.nix
    (import ../gitea-actions-runner.nix)
    (import ../gitea-runner-external-storage.nix)
    (import ../tailscale.nix {
      authKeyFile = ../../nixos/secrets/tailscale-authkey.age;
    })
  ];
  age = {
    identityPaths = [
      "${config.users.users.${primaryUser}.home}/.ssh/id_ed25519"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_rsa_key"
    ];
    secrets.gitea-runner-token.file = ../../nixos/secrets/gitea-runner-token.mac-demarco-mini.age;
  };
  services.gitea-actions-runner = {
    user = "gitea-runner";
    instances.nix = {
      enable = true;
      name = config.networking.hostName;
      url = "https://dev.railbird.ai";
      tokenFile = config.age.secrets.gitea-runner-token.path;
      labels = [
        "nix-darwin-${pkgs.stdenv.hostPlatform.system}:host"
        "macos-aarch64-darwin"
        "nix:host"
      ];
      settings = {
        cache = {
          enabled = true;
        };
        host = {
          workdir_parent = "/var/lib/gitea-runner/action-cache-dir";
        };
      };
      hostPackages = with pkgs; [
        bash
        coreutils
        curl
        direnv
        gawk
        just
        git-lfs
        isort
        git
        gnused
        ncdu
        nixVersions.stable
        nodejs
        openssh
        wget
      ];
    };
  };

  launchd.daemons.gitea-runner-nix.serviceConfig.EnvironmentVariables = {
    XDG_CONFIG_HOME = "/var/lib/gitea-runner";
    XDG_CACHE_HOME = "/var/lib/gitea-runner/.cache";
    XDG_RUNTIME_DIR = "/var/lib/gitea-runner/tmp";
  };
  users.users =
    lib.genAttrs personalUsers (user: {
      name = user;
      home = homeForUser user;
      description = mkUserDescription user;
      openssh.authorizedKeys.keys = (import ../../nix-shared/keys.nix).userKeys.${user};
    })
    // {
      gitea-runner = {
        name = "gitea-runner";
        isHidden = false;
        home = "/Users/gitea-runner";
        createHome = false;
      };
    };
}
