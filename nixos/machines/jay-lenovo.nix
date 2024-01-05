{ config, lib, pkgs, forEachUser, ... }:

{
  imports = [
    ../configuration.nix
  ];

  features.full.enable = true;

  environment.systemPackages = with pkgs; [
    android-studio
    linuxPackages_latest.perf
    zenmonitor
  ];

  hardware.enableRedistributableFirmware = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.initrd.kernelModules = [ "amdgpu" ];
  boot.initrd.availableKernelModules = [
    "nvme" "xhci_pci" "usbhid" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" "amdgpu"
  ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  modules.postgres.enable = true;

  networking.networkmanager.enable = true;

  services.xserver = {
      enable = true;
      libinput.enable = true;
      displayManager.sddm.enable = true;
      desktopManager.plasma5.enable = true;
      videoDrivers = [ "amdgpu" ];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/cb96b029-df61-45d3-905b-a9435bf446df";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/1C5A-4FBB";
    fsType = "vfat";
  };

  networking.hostName = "jay-lenovo";

  services.power-profiles-daemon.enable = false;
  services.tlp.enable = true;

  system.stateVersion = "23.05";

  home-manager.users = forEachUser {
    home.stateVersion = "23.05";
  };
}
