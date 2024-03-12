{ pkgs, config, makeEnable, ... }:
makeEnable config "modules.gitea-runner" false {
  age.secrets.gitea-runner-token = {
    file = ./secrets/gitea-runner-token.${config.networking.hostName}.age;
    group = "docker";
  };

    services.gitea-actions-runner.instances.nix =
    let gitea-runner-directory = "/var/lib/gitea-runner";
    in {
      settings = {
        cache = {
          enabled = true;
        };
        container = {
          workdir_parent = "${gitea-runner-directory}/workspace";
        };
        host = {
          workdir_parent = "${gitea-runner-directory}/action-cache-dir";
        };
      };
      hostPackages = with pkgs; [
        bash
        direnv
        coreutils
        curl
        gawk
        git-lfs
        nixFlakes
        gitFull
        gnused
        nodejs
        docker
        openssh
        wget
      ];
      enable = true;
      name = config.networking.hostName;
      url = "https://dev.railbird.ai";
      tokenFile = config.age.secrets.gitea-runner-token.path;
      labels = [
        "nixos-${pkgs.system}:host"
        "nix:docker://localhost:5921/nix-runner"
      ];
    };

    systemd.services.gitea-runner-nix.environment =
      let gitea-runner-directory = "/var/lib/gitea-runner"; in {
        XDG_CONFIG_HOME = gitea-runner-directory;
        XDG_CACHE_HOME = "${gitea-runner-directory}/.cache";
      };
    users.users.gitea-runner = {
      extraGroups = ["docker"];
      home = "/var/lib/gitea-runner";
    };
}
