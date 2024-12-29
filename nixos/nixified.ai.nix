{ inputs, config, specialArgs, ... }:
specialArgs.makeEnable config "myModules.nixified-ai" false {
  # imports = [
  #   inputs.nixified-ai.nixosModules.invokeai
  # ];

  # environment.systemPackages = [
  #   inputs.nixified-ai.packages.${config.nixpkgs.system}.invokeai-nvidia
  # ];

  # services.invokeai = {
  #   enable = false;
  #   host = "0.0.0.0";
  #   package = inputs.nixified-ai.packages.${config.nixpkgs.system}.invokeai-nvidia;
  # };
}
