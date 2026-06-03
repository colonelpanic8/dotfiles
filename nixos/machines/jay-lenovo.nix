{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../configuration.nix
  ];

  myModules.wyoming.enable = false;
  features.full.enable = true;

  environment.systemPackages = with pkgs; [
    android-studio
    perf
    zenmonitor
  ];

  hardware.enableRedistributableFirmware = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.initrd.kernelModules = ["amdgpu"];
  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "usbhid"
    "usb_storage"
    "sd_mod"
    "rtsx_pci_sdmmc"
    "amdgpu"
  ];
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 1;
  boot.loader.efi.canTouchEfiVariables = true;

  myModules.postgres.enable = true;
  myModules.kat.enable = true;
  myModules.hyprland.gaps.enable = false;
  myModules.hyprland.cursorSize = 20;
  networking.networkmanager.enable = true;
  services.libinput.enable = true;

  services.xserver = {
    enable = true;
    videoDrivers = ["amdgpu"];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/cb96b029-df61-45d3-905b-a9435bf446df";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/1C5A-4FBB";
    fsType = "vfat";
  };

  swapDevices = [
    {
      device = "/swapfile";
      size = 16384;
    }
  ];

  networking.hostName = "jay-lenovo";
  myModules.hostIdentity = {
    emoticon = "💼";
    tmux.background = "#16a34a";
  };

  services.power-profiles-daemon.enable = false;
  services.tlp.enable = true;

  system.stateVersion = "23.05";

  home-manager.sharedModules = [
    {
      home.stateVersion = "23.05";
      gtk.font.size = lib.mkForce 9;
      gtk.gtk3.extraConfig.gtk-cursor-theme-size = lib.mkForce 20;
      dconf.settings."org/gnome/desktop/interface".text-scaling-factor = 0.9;
      dconf.settings."org/gnome/desktop/interface".cursor-size = 20;
      xdg.configFile."ghostty/config".text = lib.mkAfter ''
        font-size = 11
      '';
    }
  ];
}
