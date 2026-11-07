{
  config,
  lib,
  pkgs,
  ...
}: let
  primaryUser = config.system.primaryUser;
in {
  networking.hostName = "mac-demarco-mini";
  myUsers.imalison.level = "full";
  myUsers.kat.enable = true;
  imports = [
    ../persistent-desktop.nix
    (import ../gitea-actions-runner.nix)
    (import ../gitea-runner-external-storage.nix)
    ../tailscale.nix
  ];

  # Sparkle keeps Rex updated after the cask's first install.
  nix-homebrew.taps."colonelpanic8/homebrew-local" = "${../homebrew-local}";
  homebrew.casks = ["colonelpanic8/local/rex"];

  myModules.tailscale.enrollment = "ivan";
  # Preserve this Mac's bootstrap-only authentication policy.
  myModules.tailscale.enrollmentProfiles.ivan.reauthenticateExpired = false;
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
  users.users.gitea-runner = {
    name = "gitea-runner";
    isHidden = false;
    home = "/Users/gitea-runner";
    createHome = false;
  };
}
