{ config, lib, pkgs, ... }:

{
  imports = [
    ../configuration.nix
  ];

  myModules.base.enable = true;
  myModules.desktop.enable = true;
  myModules.xmonad.enable = false;
  myModules.extra.enable = false;
  myModules.code.enable = true;
  myModules.games.enable = false;
  myModules.syncthing.enable = true;
  myModules.fonts.enable = true;
  myModules.nixified-ai.enable = false;

  hardware.enableRedistributableFirmware = true;

  # disable card with bbswitch by default since we turn it on only on demand!
  hardware.nvidiaOptimus.disable = true;

  # install nvidia drivers in addition to intel one
  hardware.opengl.extraPackages = [ pkgs.linuxPackages.nvidia_x11.out ];
  hardware.opengl.extraPackages32 = [ pkgs.linuxPackages.nvidia_x11.lib32 ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.xserver.libinput.enable = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/36864608-8e74-42b8-a075-27b59ef2701d";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/36E1-BE93";
    fsType = "vfat";
  };

  swapDevices = [
    {
      device = "/swapfile";
      priority = 0;
      size = 4096;
    }
  ];

  networking.hostName = "david-blade";

  home-manager.sharedModules = [
    {
      home.stateVersion = "24.05";
    }
  ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  system.stateVersion = "24.05";
}
