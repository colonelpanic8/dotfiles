{ config, lib, pkgs, forEachUser, ... }:

{
  imports = [
    ../configuration.nix
  ];

  modules.base.enable = true;
  modules.desktop.enable = true;
  modules.xmonad.enable = true;
  modules.extra.enable = false;
  modules.code.enable = true;
  modules.games.enable = false;
  modules.syncthing.enable = true;
  modules.fonts.enable = true;
  modules.nixified-ai.enable = false;

  hardware.enableRedistributableFirmware = true;

  # install nvidia drivers in addition to intel one
  hardware.opengl.extraPackages = [ pkgs.linuxPackages.nvidia_x11.out ];
  hardware.opengl.extraPackages32 = [ pkgs.linuxPackages.nvidia_x11.lib32 ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  hardware.nvidia.modesetting.enable = true;

  services.xserver = {
    videoDrivers = [ "nvidia" ];
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/30583504-9530-4095-a556-da1209ef9b63";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/CE95-E46C";
      fsType = "vfat";
    };

  home-manager.users = forEachUser {
    home.stateVersion = "23.11";
  };

  networking.hostName = "jimi-hendnix";

  system.stateVersion = "23.05";
}
