{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    ../full.nix
    ../base.nix
    inputs.nixos-hardware.nixosModules.dell-xps-17-9700-intel
  ];

  hardware.enableRedistributableFirmware = true;

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];

  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = with config.boot.kernelPackages; [ ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.xserver.libinput.enable = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/bfe4586b-2538-4aae-ad2f-b1277378de4a";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/5C09-F06F";
      fsType = "vfat";
    };

  swapDevices = [ ];

  networking.hostName = "adele";

  nix.settings.maxJobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  services.redshift.enable = true;

  location = {
    latitude = 37.8104601;
    longitude = -122.2572529;
  };

  system.stateVersion = "20.03";
}
