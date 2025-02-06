{ lib, pkgs, inputs, forEachUser, ... }:

{
  imports = [
    ../configuration.nix
    inputs.nixos-hardware.nixosModules.dell-xps-17-9700-nvidia
  ];

  myModules.wyoming.enable = true;
  myModules.base.enable = true;
  myModules.desktop.enable = true;
  myModules.xmonad.enable = true;
  myModules.extra.enable = false;
  myModules.code.enable = true;
  myModules.games.enable = false;
  myModules.syncthing.enable = true;
  myModules.fonts.enable = true;
  myModules.nixified-ai.enable = false;
  myModules.gitea-runner.enable = false;
  hardware.nvidia.open = true;

  hardware.enableRedistributableFirmware = true;

  environment.systemPackages = with pkgs; [
    android-studio
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  services.xserver = {
    videoDrivers = [ "nvidia" ];
  };
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.libinput = {
    enable = true;
    touchpad.tapping = true;
  };

  security.pam.services.login.fprintAuth = true;
  services.fprintd = {
    enable = true;
    tod = {
      enable = true;
      driver = pkgs.libfprint-2-tod1-goodix;
    };
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/18af5b4c-69c7-41a8-865e-bc3f5269d2f9";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/8A9F-D7D2";
      fsType = "vfat";
    };

  swapDevices = [ ];

  networking.hostName = "adell";

  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";

  home-manager.users = forEachUser {
    home.stateVersion = "23.05";
  };

  system.stateVersion = "23.05";
}
