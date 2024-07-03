{ config, lib, pkgs, inputs, forEachUser, ... }:

{
  imports = [
    ../configuration.nix
    inputs.nixos-hardware.nixosModules.asus-rog-strix-g834jzr
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
  modules.gitea-runner.enable = false;

  hardware.enableRedistributableFirmware = true;

  environment.systemPackages = with pkgs; [
    android-studio
  ];

  services.xserver.dpi = 96;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.availableKernelModules = [ "vmd" "xhci_pci" "thunderbolt" "nvme" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.asusd.enable = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/fc06a54c-cc45-423a-914b-8dfcb5939106";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/B28A-829A";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/27f277a0-b552-43a0-904d-625e48922bb9"; }
    ];

  networking.hostName = "strixi-minaj";

  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  home-manager.users = forEachUser {
    home.stateVersion = "23.05";
  };

  system.stateVersion = "23.05";
}
