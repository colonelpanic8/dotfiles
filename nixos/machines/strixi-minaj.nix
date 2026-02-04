{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    ../configuration.nix
    inputs.nixos-hardware.nixosModules.asus-rog-strix-g834jzr
  ];

  myModules.base.enable = true;
  myModules.desktop.enable = true;
  myModules.xmonad.enable = true;
  myModules.extra.enable = false;
  myModules.code.enable = true;
  myModules.games.enable = false;
  myModules.syncthing.enable = true;
  myModules.fonts.enable = true;
  myModules.gitea-runner.enable = false;
  myModules.nvidia.enable = true;
  myModules.electron.enable = true;
  myModules.wyoming.enable = false;
  myModules.tts.enable = false;

  hardware.enableRedistributableFirmware = true;

  # nixpkgs.config.cudaSupport = true;

  boot.loader.systemd-boot.configurationLimit = 5;

  environment.systemPackages = with pkgs; [
    android-studio
    gimp
    inkscape
  ];

  services.synergy.server = {
    enable = true;
    autoStart = true;
    configFile = ../../dotfiles/synergy.conf;
  };

  services.matter-server = {
    enable = false;
    logLevel = "debug";
    extraArgs = ["--bluetooth-adapter=0" "--enable-test-net-dcl"];
  };

  programs.virt-manager.enable = true;
  virtualisation.libvirtd.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;

  services.xserver.dpi = 96;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # See https://github.com/NixOS/nixpkgs/issues/467814 for why this was needed
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;
  boot.initrd.availableKernelModules = [ "vmd" "xhci_pci" "thunderbolt" "nvme" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "nvidia" "nvidia_drm" "nvidia_uvm" "nvidia_modeset" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  hardware.nvidia.powerManagement.enable = true;
  hardware.nvidia.prime.offload.enable = lib.mkForce false;
  hardware.nvidia.prime.sync.enable = lib.mkForce true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.asusd.enable = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/fc06a54c-cc45-423a-914b-8dfcb5939106";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/B28A-829A";
    fsType = "vfat";
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/27f277a0-b552-43a0-904d-625e48922bb9"; }
    { device = "/swapfile"; size = 16384; } # size is in MiB (adds 16 GiB)
  ];

  networking.hostName = "strixi-minaj";

  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault true;

  home-manager.sharedModules = [
    {
      home.stateVersion = "23.05";
      xdg.configFile."waybar/disks".text = ''
        # One mountpoint per line (comments with # are ignored).
        /
      '';
    }
  ];

  system.stateVersion = "23.05";
}
