{ config, lib, pkgs, ... }:

{
  imports = [
    ../users.nix
    ../full.nix
  ];

  hardware.enableRedistributableFirmware = true;

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.loader.systemd-boot.enable = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/110e3bf8-19b7-4a39-8e2a-b4c3c0d59d0e";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/B2DA-CD21";
    fsType = "vfat";
  };

  fileSystems."/windows" = {
    device = "/dev/disk/by-uuid/DEFA1F27FA1EFC09";
    fsType = "ntfs";
  };

  fileSystems."/arch-linux-root" = {
    device = "/dev/disk/by-uuid/9095e51e-33f9-440d-a272-46e129800f81";
    fsType = "ext4";
  };

  fileSystems."/backups" = {
    device = "/dev/disk/by-uuid/752E628C67697A81";
    fsType = "ntfs";
  };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/a6933b22-c7f4-4c57-b330-1450f313922e"; }
      { device = "/dev/disk/by-uuid/dbd49a76-4b3e-4b5c-9a88-68a9e61f6210"; }
    ];

  nix.settings.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  # boot.initrd.luks.devices."cryptroot" = {
  #   name = "root";
  #   device = "/dev/sda3";
  #   preLVM = true;
  # };

  networking.hostName = "imalison-home";
  boot.loader.efi.canTouchEfiVariables = true;

  services.xserver = {
    screenSection = ''
      DefaultDepth 24
      Option "RegistryDwords" "PerfLevelSrc=0x3322; PowerMizerDefaultAC=0x1"
      Option "TripleBuffer" "True"
      Option "Stereo" "0"
      Option "nvidiaXineramaInfoOrder" "DFP-0"
      Option "metamodes" "DVI-D-0: nvidia-auto-select +0+2160 {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}, HDMI-0: nvidia-auto-select +640+0 {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}, HDMI-1: nvidia-auto-select +2560+2160 {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}"
      Option "SLI" "Off"
      Option "MultiGPU" "Off"
      Option "BaseMosaic" "off"
    '';
    videoDrivers = [ "nvidia" ];
  };
}
