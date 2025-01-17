{ config, lib, pkgs, forEachUser, ... }:

{
  imports = [
    ../configuration.nix
  ];

  myModules.fonts.enable = true;
  myModules.base.enable = true;
  myModules.desktop.enable = true;
  myModules.xmonad.enable = true;

  services.matter-server = {
    enable = true;
  };

  services.home-assistant = {
    enable = true;
    config = {
      extraComponents = [
        "esphome"
        "met"
        "radio_browser"
      ];
      config = {
        default_config = {};
        met = {};
      };
    };
  };

  boot.loader.systemd-boot.configurationLimit = 3;

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.loader.systemd-boot.enable = true;

  # Add Intel Wi-Fi firmware
  hardware.enableRedistributableFirmware = true;
  hardware.enableAllFirmware = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/453d28a1-26f2-4b25-ac72-c6d301fd0bb8";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/184E-E5E8";
      fsType = "vfat";
    };

  swapDevices = [ ];

  networking.hostName = "justin-bieber-creek";

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  system.stateVersion = "23.05";

  home-manager.users = forEachUser {
    home.stateVersion = "23.05";
  };
}
