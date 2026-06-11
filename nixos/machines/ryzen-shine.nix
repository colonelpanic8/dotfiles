{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    inputs.grub2-themes.nixosModules.default
    ../configuration.nix
    ../nixified.ai.nix
  ];

  features.full.enable = true;
  myModules.games.enable = lib.mkForce true;
  myModules.kubelet.enable = false;
  myModules.nvidia.enable = true;
  # Needed for now because monitors have different refresh rates
  myModules.xmonad.picom.vSync.enable = false;
  myModules.cache-server = {
    enable = true;
    port = 3090;
  };
  myModules.gitea-runner.enable = false;
  myModules.postgres.enable = true;
  myModules.tts.enable = true;
  myModules.cua = {
    enable = true;
    android = {
      enable = true;
      # Android is QEMU/KVM-only. Keep this manually startable until SVM/AMD-V
      # is exposed to Linux and /dev/kvm exists on this host.
      autoStart = false;
    };
  };
  myModules.railbird-k3s = {
    enable = false;
    serverAddr = "https://jimi-hendnix.local:6443";
    # extraFlags = ["--node-taint preferNoSchedule=true:NoSchedule"];
  };
  services.mullvad-vpn.enable = lib.mkForce false;
  myModules.nixified-ai.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader.systemd-boot.configurationLimit = 5;
  myModules.bootloaders.systemdBoot.enable = false;
  myModules.bootloaders.grub = {
    enable = true;
    windowsEfiUuid = "B270-C7E6";
    gfxmode = "3440x1440,auto";
  };
  boot.loader.grub2-theme = {
    enable = true;
    theme = "whitesur";
    icon = "whitesur";
    screen = "ultrawide2k";
  };

  networking.hostName = "ryzen-shine";
  myModules.hostIdentity = {
    emoticon = "☀️";
    tmux.background = "#2563eb";
  };

  environment.systemPackages = with pkgs; [
    perf
  ];

  boot.initrd.systemd.enable = true;
  boot.plymouth = {
    enable = false;
  };

  hardware.enableRedistributableFirmware = true;

  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];

  hardware.nvidia.modesetting.enable = true;

  # Pin the NVIDIA driver to 595.71.05 on this host only. nixpkgs' production
  # driver moved 595.71.05 -> 595.80 (nixpkgs 9b366138, 2026-06-02), and 595.80
  # introduced a GSP-firmware regression on this RTX 3070 Ti (GA104): random hard
  # freezes with "GSP RM heartbeat timed out" / Xid 119 GSP RPC timeouts (see
  # boots from 2026-06-08 onward). 595.71.05 ran cleanly for 4.5d before the bump,
  # and the open module can't disable GSP, so we pin the last-good build directly.
  # Hashes lifted from the parent of the nixpkgs bump commit. Revisit once a newer
  # driver branch (e.g. the 610.x new_feature branch) is confirmed stable here.
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
    version = "595.71.05";
    sha256_64bit = "sha256-NiA7iWC35JyKQva6H1hjzeNKBek9KyS3mK8G3YRva4I=";
    sha256_aarch64 = "sha256-XzKloS00dFKTd4ATWkTIhm9eG/OzR/Sim6MboNZWPu8=";
    openSha256 = "sha256-Lfz71QWKM6x/jD2B22SWpUi7/og30HRlXg1kL3EWzEw=";
    settingsSha256 = "sha256-mXnf3jyvznfB3OfKd657rxv0rYHQb/dX/Riw/+N9EKU=";
    persistencedSha256 = "sha256-Z/6IvEEa/XfZ5F5qoSIPvXJLGtscYVqjFxHZaN/M2Ts=";
  };

  hardware.graphics.enable32Bit = true;

  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/9bd06145-8151-4f7b-bcfe-f1809e0db1ea";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/E1E1-909E";
    fsType = "vfat";
  };

  fileSystems."/shared" = {
    device = "/dev/disk/by-uuid/D4009CE8009CD33A";
    fsType = "ntfs";
    options = ["nofail" "uid=0" "gid=users" "umask=002"];
  };

  swapDevices = [
    {
      device = "/swapfile";
      size = 49152;
    }
  ];

  # nix.settings.maxJobs = lib.mkDefault 16;
  # High-DPI console
  console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

  # services.xrdp.enable = true;
  # services.xrdp.defaultWindowManager = "startplasma-x11";
  # services.xrdp.openFirewall = true;

  system.autoUpgrade = {
    enable = true;
    dates = "daily";
  };

  system.stateVersion = "20.03";
  home-manager.sharedModules = [
    {
      home.stateVersion = "21.05";
      gtk.font.size = lib.mkForce 11;
      dconf.settings."org/gnome/desktop/interface".text-scaling-factor = 1.05;
    }
  ];

  # users.extraUsers.dean.home = "/shared/dean";
}
