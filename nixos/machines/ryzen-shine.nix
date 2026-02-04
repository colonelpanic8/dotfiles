{ lib, pkgs, ... }:

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
    serverAddr = "https://jimi-hendnix.local:6443";
    # extraFlags = ["--node-taint preferNoSchedule=true:NoSchedule"];
  };
  myModules.nixified-ai.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

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

  hardware.nvidia.modesetting.enable = true;

  hardware.graphics.enable32Bit = true;

  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

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
    options = [ "nofail" "uid=0" "gid=users" "umask=002" ];
  };

  # nix.settings.maxJobs = lib.mkDefault 16;
  # High-DPI console
  console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

  # services.xrdp.enable = true;
  # services.xrdp.defaultWindowManager = "startplasma-x11";
  # services.xrdp.openFirewall = true;

  system.stateVersion = "20.03";
  home-manager.sharedModules = [
    {
      home.stateVersion = "21.05";
    }
  ];

  # users.extraUsers.dean.home = "/shared/dean";
}
