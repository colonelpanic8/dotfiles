{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ../configuration.nix
  ];

  services.macos-ventura = {
    enable = false;
    openFirewall = true;
    vncListenAddr = "0.0.0.0";
  };

  features.full.enable = true;
  myModules.taffybar.aiUsageMode = "both";
  myModules.cache-server = {
    enable = true;
    port = 3090;
  };
  myModules.gitea-runner.enable = false;
  myModules.vscode.enable = true;
  myModules.kat.enable = true;
  myModules.nvidia.enable = true;
  myModules.hyprland.ultrawideRefreshRate = 99.98;
  myModules.railbird-k3s = {
    enable = false;
    serverAddr = "https://dev.railbird.ai:6443";
  };
  services.k3s.enable = lib.mkForce false;

  networking.hostName = "jimi-hendnix";
  myModules.hostIdentity = {
    emoticon = "🐩";
    tmux.background = "#db2777";
  };

  hardware.enableRedistributableFirmware = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];
  boot.loader.systemd-boot.enable = true;

  hardware.nvidia.modesetting.enable = true;

  # This also enables v4l2loopback
  programs.droidcam.enable = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/64a7c1f5-727a-413c-81a2-cb108728cff6";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/EE25-DC15";
    fsType = "vfat";
  };

  # Former system root; keep it read-only until its ext4 errors are repaired.
  fileSystems."/mnt/old-nixos" = {
    device = "/dev/disk/by-id/nvme-WD_BLACK_SN770_1TB_233216802763-part1";
    fsType = "ext4";
    options = [
      "ro"
      "noload"
      "nofail"
      "x-systemd.automount"
      "x-systemd.device-timeout=1s"
      "x-systemd.mount-timeout=5s"
    ];
  };

  fileSystems."/mnt/windows" = {
    device = "/dev/disk/by-uuid/DEFA1F27FA1EFC09";
    fsType = "ntfs3";
    options = [
      "ro"
      "uid=1000"
      "gid=100"
      "umask=0022"
      "nofail"
      "x-systemd.automount"
      "x-systemd.device-timeout=1s"
      "x-systemd.mount-timeout=5s"
    ];
  };

  fileSystems."/mnt/sata-nixos" = {
    device = "/dev/disk/by-uuid/30583504-9530-4095-a556-da1209ef9b63";
    fsType = "ext4";
    options = [
      "ro"
      "noload"
      "nofail"
      "x-systemd.automount"
      "x-systemd.device-timeout=1s"
      "x-systemd.mount-timeout=5s"
    ];
  };

  # Prefer the SN770; the Intel 660p is QLC and shares its device with /.
  swapDevices = [
    {
      device = "/dev/disk/by-uuid/598e9aa1-4940-4410-a2fa-3dfd8b7d2c0d";
      priority = 10;
    }
    {
      device = "/dev/disk/by-uuid/c0dcff59-8072-48fb-b242-a7a1797e4b48";
      priority = 1;
    }
  ];

  # Absorb swap traffic in RAM before touching either disk.
  zramSwap = {
    enable = true;
    algorithm = "zstd";
    priority = 100;
  };

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  services.xrdp.enable = true;
  services.xrdp.defaultWindowManager = "startplasma-x11";
  services.xrdp.openFirewall = true;

  system.autoUpgrade = {
    enable = true;
    dates = "hourly";
  };

  home-manager.sharedModules = [
    {
      home.stateVersion = "23.11";
    }
  ];

  system.stateVersion = "23.11";
}
