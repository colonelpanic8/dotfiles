{ inputs, config, ... }: {
  imports = [
    inputs.nixified-ai.nixosModules.invokeai
  ];

  environment.systemPackages = [
    inputs.nixified-ai.packages.${config.nixpkgs.system}.invokeai-nvidia
  ];

  services.invokeai = {
    enable = false;
    host = "0.0.0.0";
    nsfwChecker = false;
    package = inputs.nixified-ai.packages.${config.nixpkgs.system}.invokeai-nvidia;
  };
}
