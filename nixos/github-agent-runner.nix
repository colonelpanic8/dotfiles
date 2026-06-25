{
  pkgs,
  config,
  makeEnable,
  ...
}:
makeEnable config "myModules.githubAgentRunner" false {
  age.secrets.github-rocket-sense-runner-token = {
    file = ./secrets/github-rocket-sense-runner-token.age;
    mode = "0400";
  };

  services.github-runners.rocket-sense-agent = {
    enable = true;
    url = "https://github.com/rlrml/rocket-sense";
    name = "${config.networking.hostName}-rocket-sense-agent";
    tokenFile = config.age.secrets.github-rocket-sense-runner-token.path;
    tokenType = "registration";
    replace = true;
    ephemeral = false;
    user = "imalison";
    extraLabels = [
      "rocket-sense-agent"
      "nixos"
      "codex"
      "claude-code"
    ];
    extraPackages = with pkgs; [
      bashInteractive
      cargo
      claude-code
      codex
      coreutils
      curl
      direnv
      gh
      git-lfs
      gitFull
      gnugrep
      gnused
      jq
      just
      nixVersions.stable
      nodejs
      openssh
      pkg-config
      ripgrep
      rustc
      zsh
    ];
    extraEnvironment = {
      CODEX_HOME = "/home/imalison/.codex";
      NIX_CONFIG = "experimental-features = nix-command flakes";
    };
    serviceOverrides = {
      ProtectHome = false;
      ProtectSystem = "full";
      PrivateTmp = false;
      PrivateUsers = false;
      RestrictNamespaces = false;
    };
  };
}
