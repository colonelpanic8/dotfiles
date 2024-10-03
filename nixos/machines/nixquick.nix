{ config, lib, pkgs, inputs, forEachUser, ... }:
{
  imports = [
    ../configuration.nix
  ];

  services.macos-ventura = {
    enable = true;
    openFirewall = true;
    vncListenAddr = "0.0.0.0";
  };

  features.full.enable = true;
  myModules.cache-server = {
    enable = true;
    port = 3090;
  };
  myModules.gitea-runner.enable = false;
  myModules.vscode.enable = true;
  myModules.kat.enable = true;
  myModules.nvidia.enable = true;
  myModules.railbird-k3s = {
    enable = true;
    serverAddr = "https://dev.railbird.ai:6443";
  };

  networking.hostName = "nixquick";

  hardware.enableRedistributableFirmware = true;
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.loader.systemd-boot.enable = true;

  hardware.nvidia.modesetting.enable = true;

  # This also enables v4l2loopback
  programs.droidcam.enable = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/64a7c1f5-727a-413c-81a2-cb108728cff6";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/EE25-DC15";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/c0dcff59-8072-48fb-b242-a7a1797e4b48"; }
    ];


  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  services.xrdp.enable = true;
  services.xrdp.defaultWindowManager = "startplasma-x11";
  services.xrdp.openFirewall = true;

  home-manager.users = forEachUser {
    home.stateVersion = "23.11";
  };

  system.stateVersion = "23.11";
}
