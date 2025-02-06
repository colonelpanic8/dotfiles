{ config, lib, pkgs, inputs, forEachUser, ... }:

{
  imports = [
    ../configuration.nix
    inputs.nixos-hardware.nixosModules.asus-rog-strix-g834jzr
  ];

  hardware.nvidia.open = true;
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
  myModules.wyoming.enable = true;

  hardware.enableRedistributableFirmware = true;

  environment.systemPackages = with pkgs; [
    android-studio
  ];

  services.synergy.server = {
    enable = true;
    autoStart = true;
    configFile = ../../dotfiles/synergy.conf;
  };

  services.matter-server = {
    enable = true;
    logLevel = "debug";
    extraArgs = ["--bluetooth-adapter=0" "--enable-test-net-dcl"];
  };

  programs.virt-manager.enable = true;
  virtualisation.libvirtd.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;

  virtualisation.virtualbox.host = {
    enable = true;
    enableKvm = true;
    addNetworkInterface = false;
  };

  services.xserver.dpi = 96;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.availableKernelModules = [ "vmd" "xhci_pci" "thunderbolt" "nvme" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "nvidia" "nvidia_drm" "nvidia_uvm" "nvidia_modeset" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  hardware.nvidia.powerManagement.enable = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.asusd.enable = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/fc06a54c-cc45-423a-914b-8dfcb5939106";
    fsType = "ext4";
    };

    fileSystems."/boot" =
      { device = "/dev/disk/by-uuid/B28A-829A";
      fsType = "vfat";
      };

      swapDevices =
        [ { device = "/dev/disk/by-uuid/27f277a0-b552-43a0-904d-625e48922bb9"; }
        ];

        networking.hostName = "strixi-minaj";

        powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
        hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

        home-manager.users = forEachUser {
          home.stateVersion = "23.05";
        };

        system.stateVersion = "23.05";
}
