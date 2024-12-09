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
  services.xserver = {
    videoDrivers = [ "nvidia" ];
  };
}
