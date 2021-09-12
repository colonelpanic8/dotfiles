{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    ../full.nix
    ../base.nix
    inputs.nixos-hardware.nixosModules.dell-xps-17-9700-nvidia
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
    { device = "/dev/disk/by-uuid/7c999009-1ff1-42f7-a64a-3fa91fc777a8";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."cryptroot".device = "/dev/disk/by-uuid/97c1eee7-b161-4186-9c14-6b1771d49afb";

  fileSystems."/boot" =
    { device = "/dev/disk/by-label/ESP";
      fsType = "vfat";
    };


  swapDevices = [ ];

  networking.hostName = "stevie-nixos";

  nix.maxJobs = lib.mkDefault 16;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  hardware.video.hidpi.enable = true;

  system.stateVersion = "21.05";
}
