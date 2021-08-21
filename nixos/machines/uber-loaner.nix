{ config, lib, pkgs, ... }:

{
  imports = [
    ../full.nix
    ../games.nix
    ../extra.nix
  ];

  hardware.enableRedistributableFirmware = true;

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ pkgs.linuxPackages.rtl8814au ];
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.enable = true;
  services.xserver = {
    libinput.enable = true;
  };
  hardware.facetimehd.enable = true;

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/EFI";
    fsType = "vfat";
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/2958f04b-8387-4a0c-abc1-f12036c53581";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/33c38b23-af1a-4bc4-913e-c774f1030817"; }
    ];

  fileSystems."/tmp" =
    { device = "tmpfs";
      fsType = "tmpfs";
    };

  networking.hostName = "uber-loaner";

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  users.extraUsers.root.initialHashedPassword = "";
}
