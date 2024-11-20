{ config, lib, pkgs, inputs, forEachUser, ... }:

{
  imports = [
    ../configuration.nix
  ];

  features.full.enable = true;
  myModules.kubelet.enable = false;
  myModules.nvidia.enable = true;
  # Needed for now because monitors have different refresh rates
  myModules.xmonad.picom.vSync.enable = false;
  myModules.cache-server = {
    enable = true;
    port = 3090;
  };
  myModules.gitea-runner.enable = true;
  myModules.postgres.enable = true;
  myModules.railbird-k3s = {
    enable = true;
    # extraFlags = ["--node-taint preferNoSchedule=true:NoSchedule"];
  };

  boot.loader.systemd-boot.configurationLimit = 5;

  networking.hostName = "ryzen-shine";

  environment.systemPackages = with pkgs; [
    linuxPackages_latest.perf
  ];

  boot.initrd.systemd.enable = true;
  boot.plymouth = {
    enable = false;
  };

  services.autorandr = {
    enable = true;
  };

  hardware.enableRedistributableFirmware = true;

  networking.interfaces.enp5s0.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.luks.devices."cryptroot".device = "/dev/nvme0n1p5";
  boot.initrd.kernelModules = [ "dm-snapshot" ];

  # install nvidia drivers in addition to intel one
  hardware.graphics.extraPackages = [ pkgs.linuxPackages.nvidia_x11.out ];
  hardware.graphics.extraPackages32 = [ pkgs.linuxPackages.nvidia_x11.lib32 ];
  services.xserver = {
    videoDrivers = [ "nvidia" ];
  };

  hardware.nvidia.modesetting.enable = true;

  hardware.graphics.enable32Bit = true;

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
    options = [ "nofail" "uid=0" "gid=users" "umask=002" ];
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/f719b44e-295a-4909-9a60-84f87acb7f77"; }
  ];

  # nix.settings.maxJobs = lib.mkDefault 16;
  # High-DPI console
  console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

  # services.xrdp.enable = true;
  # services.xrdp.defaultWindowManager = "startplasma-x11";
  # services.xrdp.openFirewall = true;

  system.stateVersion = "20.03";
  home-manager.users = forEachUser {
    home.stateVersion = "21.05";
  };

  # users.extraUsers.dean.home = "/shared/dean";
}
