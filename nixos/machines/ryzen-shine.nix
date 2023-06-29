{ config, lib, pkgs, ... }:

{
  imports = [
    ../full.nix
  ];

  networking.hostName = "ryzen-shine";

  environment.systemPackages = with pkgs; [
    linuxPackages_latest.perf
  ];

  hardware.enableRedistributableFirmware = true;

  networking.interfaces.enp5s0.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.luks.devices."cryptroot".device = "/dev/nvme0n1p5";
  boot.initrd.kernelModules = [ "dm-snapshot" ];

  # install nvidia drivers in addition to intel one
  hardware.opengl.extraPackages = [ pkgs.linuxPackages.nvidia_x11.out ];
  hardware.opengl.extraPackages32 = [ pkgs.linuxPackages.nvidia_x11.lib32 ];
  services.xserver = {
    videoDrivers = [ "nvidia" ];
  };

  hardware.nvidia.modesetting.enable = true;

  hardware.opengl.driSupport32Bit = true;

  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/356173ab-d076-43e0-aeb6-6a6829c4402b";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/B270-C7E6";
    fsType = "vfat";
  };

  fileSystems."/shared" = {
    device = "/dev/disk/by-uuid/D4009CE8009CD33A";
    fsType = "ntfs";
    options = [ "nofail" ];
  };

  swapDevices =[
    { device = "/dev/disk/by-uuid/f719b44e-295a-4909-9a60-84f87acb7f77"; }
  ];

  # nix.settings.maxJobs = lib.mkDefault 16;
  # High-DPI console
  console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

  system.stateVersion = "20.03";
}
