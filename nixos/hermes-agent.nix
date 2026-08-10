{
  config,
  inputs,
  makeEnable,
  pkgs,
  ...
}:
makeEnable config "myModules.hermesAgent" false {
  imports = [inputs.hermes-agent.nixosModules.default];

  age.secrets.hermes-environment = {
    file = ./secrets/hermes-environment.age;
    owner = "imalison";
    group = "users";
    mode = "0400";
  };

  services.hermes-agent = {
    enable = true;
    user = "imalison";
    group = "users";
    createUser = false;
    stateDir = "/home/imalison/.local/share/hermes";
    workingDirectory = "/home/imalison/Projects";
    addToSystemPackages = true;

    settings = {
      model = {
        provider = "openai-codex";
        default = "gpt-5.6-terra";
      };
      toolsets = ["all"];
      approvals.mode = "smart";
      terminal = {
        backend = "local";
        timeout = 1800;
      };
    };

    environment = {
      API_SERVER_ENABLED = "true";
      API_SERVER_HOST = "100.90.1.42";
      API_SERVER_PORT = "8642";
    };
    environmentFiles = [config.age.secrets.hermes-environment.path];

    extraPackages = with pkgs; [
      curl
      findutils
      gh
      gnugrep
      gnused
      jq
      nix
      openssh
      ripgrep
      zsh
    ];
  };

  systemd.services.hermes-agent = {
    after = ["agenix.service" "tailscaled.service"];
    wants = ["tailscaled.service"];
  };

  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [8642];
}
