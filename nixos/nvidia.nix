{ config, pkgs, makeEnable, lib, ... }:

makeEnable config "myModules.nvidia" false {
  environment.systemPackages = with pkgs; [
    nvidia-container-toolkit
    nvidia-container-toolkit.tools
  ];
  hardware.nvidia-container-toolkit = {
    enable = true;
    mount-nvidia-executables = true;
    # additionalEdit = ''
    #   ${lib.getExe pkgs.jq} '
    #       .devices |= map(
    #       .containerEdits.hooks |= map(select(.args | index("nvidia-cdi-hook") < 0))
    #       )' | ${lib.getExe pkgs.jq} '.containerEdits.hooks |= map(select(.args | index("nvidia-cdi-hook") < 0 ))' '';
  };
  hardware.nvidia.open = true;
  hardware.graphics.enable32Bit = true;
  hardware.graphics.extraPackages = [ config.boot.kernelPackages.nvidia_x11.out ];
  hardware.graphics.extraPackages32 = [ config.boot.kernelPackages.nvidia_x11.lib32 ];
  services.xserver = {
    videoDrivers = [ "nvidia" ];
  };
  # nixpkgs.config.cudaSupport = true;
}
