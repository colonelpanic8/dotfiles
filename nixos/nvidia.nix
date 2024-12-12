{ config, pkgs, makeEnable, ... }:

makeEnable config "myModules.nvidia" false {
  environment.systemPackages = with pkgs; [
    nvidia-container-toolkit
  ];
  hardware.nvidia-container-toolkit = {
    enable = true;
    mount-nvidia-executables = false;
  };
  virtualisation.docker.enableNvidia = true;
  hardware.nvidia.open = true;
  hardware.graphics.enable32Bit = true;
  hardware.graphics.extraPackages = [ config.boot.kernelPackages.nvidia_x11.out ];
  hardware.graphics.extraPackages32 = [ config.boot.kernelPackages.nvidia_x11.lib32 ];
  services.xserver = {
    videoDrivers = [ "nvidia" ];
  };
}
