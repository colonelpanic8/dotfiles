{
  pkgs,
  config,
  makeEnable,
  ...
}:
makeEnable config "myModules.gitea-runner" false {
  age.secrets.gitea-runner-token = {
    file = ./secrets/gitea-runner-token.age;
    group = "docker";
  };

  services.gitea-actions-runner.instances.nix = let
    gitea-runner-directory = "/var/lib/gitea-runner";
  in {
    settings = {
      runner = {
        capacity = 4;
      };
      cache = {
        enabled = true;
      };
      container = {
        workdir_parent = "${gitea-runner-directory}/workspace";
        # Increase shared memory for containers (default 64MB is too small for Metro/Gradle)
        options = "--shm-size=2g";
      };
      host = {
        workdir_parent = "${gitea-runner-directory}/action-cache-dir";
      };
    };
    hostPackages = with pkgs; [
      bash
      coreutils
      curl
      direnv
      docker
      gawk
      git-lfs
      gitFull
      gnused
      just
      nixVersions.stable
      nodejs
      openssh
      wget
    ];
    enable = true;
    name = config.networking.hostName;
    url = "https://dev.railbird.ai";
    tokenFile = config.age.secrets.gitea-runner-token.path;
    labels = [
      "nixos-${pkgs.stdenv.hostPlatform.system}:host"
      "nix:docker://localhost:5921/nix-runner"
    ];
  };

  systemd.services.gitea-runner-nix = {
    environment = let
      gitea-runner-directory = "/var/lib/gitea-runner";
    in {
      XDG_CONFIG_HOME = gitea-runner-directory;
      XDG_CACHE_HOME = "${gitea-runner-directory}/.cache";
      # Despite PrivateTmp=false below, the upstream module's DynamicUser=true
      # still gives the service a private /tmp: a tmpfs capped at 10% of RAM
      # (~3.2G here) with 400k inodes. CI jobs inherit it — `nix develop`
      # shell dirs (leaked on cancelled jobs) and pytest's ephemeral postgres
      # (tempfile.mkdtemp) slowly fill it until tests die with
      # DiskFull/ENOSPC while `df /` shows hundreds of GB free. Point job
      # temp at the disk-backed state dir instead; tmpfiles rule below prunes
      # leaked entries.
      TMPDIR = "${gitea-runner-directory}/tmp";
    };
    serviceConfig.PrivateTmp = false;
  };
  systemd.tmpfiles.rules = [
    "d /var/lib/private/gitea-runner/tmp 0750 gitea-runner gitea-runner 2d"
  ];
  users.groups.gitea-runner = {};
  users.users.gitea-runner = {
    isSystemUser = true;
    group = "gitea-runner";
    extraGroups = ["docker"];
    home = "/var/lib/gitea-runner";
  };
}
