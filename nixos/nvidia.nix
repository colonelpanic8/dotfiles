{ config, pkgs, makeEnable, ... }:

makeEnable config "myModules.nvidia" false {
  environment.systemPackages = with pkgs; [
    nvidia-container-toolkit
  ];
  hardware.nvidia-container-toolkit = {
    enable = true;
    mount-nvidia-executables = true;
  };
  hardware.nvidia.open = false;
  hardware.graphics.extraPackages = [ pkgs.linuxPackages.nvidia_x11.out ];
  hardware.graphics.extraPackages32 = [ pkgs.linuxPackages.nvidia_x11.lib32 ];
  hardware.graphics.enable32Bit = true;
  services.xserver = {
    videoDrivers = [ "nvidia" ];
  };
}
