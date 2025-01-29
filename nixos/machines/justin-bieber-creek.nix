{ config, lib, pkgs, forEachUser, ... }:

{
  imports = [
    ../configuration.nix
  ];

  myModules.fonts.enable = true;
  myModules.base.enable = true;
  myModules.desktop.enable = true;
  myModules.xmonad.enable = true;

  networking.enableIPv6 = true;

  boot.kernel.sysctl = {
    # For all interfaces (e.g. if you want to accept RA on all):
    "net.ipv6.conf.all.accept_ra" = lib.mkForce "1";
    "net.ipv6.conf.all.accept_ra_rt_info_max_plen" = lib.mkForce "64";
    "net.ipv6.conf.default.accept_ra" = lib.mkForce "1";
    "net.ipv6.conf.default.accept_ra_rt_info_max_plen" = lib.mkForce "64";
    "net.ipv6.conf.wlo1.accept_ra" = lib.mkForce "1";
    "net.ipv6.conf.wlo1.accept_ra_rt_info_max_plen" = lib.mkForce "64";

    # Ensure forwarding is off on all interfaces unless needed
    "net.ipv6.conf.all.forwarding" = lib.mkForce "0";
  };

  systemd.services.otbr-agent = {
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
  };

  services.openthread-border-router = {
    enable = true;
    backboneInterface = "wpan0";
    logLevel = "debug";
    radio =  {
      device = "/dev/serial/by-id/usb-Nabu_Casa_Home_Assistant_Connect_ZBT-1_0cd053abfa38ef119c66e1d154516304-if00-port0";
      baudRate = 460800;
      flowControl = true;
    };
    web = {
      listenPort = 8087;
    };
    rest = {
      listenPort = 8089;
    };
  };

  services.matter-server = {
    enable = true;
    logLevel = "debug";
    extraArgs = ["--bluetooth-adapter=0"];
  };

  services.home-assistant = {
    enable = true;
    extraComponents = [
      "anthropic"
      "cast"
      "dlna_dmr"
      "esphome"
      "google_assistant"
      "homeassistant_hardware"
      "homeassistant_sky_connect"
      "homekit_controller"
      "ibeacon"
      "isal"
      "kef"
      "kegtron"
      "matter"
      "met"
      "opensky"
      "otbr"
      "radio_browser"
      "roomba"
      "samsungtv"
      "thread"
      "webostv"
      "yale"
    ];
    extraPackages = python3Packages: with python3Packages; [
      numpy
      python-matter-server
      universal-silabs-flasher
    ];
    config = {
      default_config = {};
    };
  };

  boot.loader.systemd-boot.configurationLimit = 3;

  boot.kernelPackages = pkgs.linuxPackages_latest;

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

  networking.useDHCP = false;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  system.stateVersion = "23.05";

  home-manager.users = forEachUser {
    home.stateVersion = "23.05";
  };
}
