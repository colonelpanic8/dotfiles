{ config, lib, pkgs, forEachUser, ... }:
{
  imports = [
    ../configuration.nix
  ];

  networking.hostName = "railbird-sf";

  hardware.enableRedistributableFirmware = true;
  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];
  boot.loader.systemd-boot.enable = true;
  modules.postgres.enable = true;

  services.k3s.role = "agent";
  services.k3s.extraFlages = lib.mkForce ["--node-label nixos-nvidia-cdi=enabled"];

  hardware.nvidia = {
    powerManagement.enable = false;
    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Enable the Nvidia settings menu,
	  # accessible via `nvidia-settings`.
    nvidiaSettings = true;
  };

  features.full.enable = false;
  myModules.base.enable = true;
  myModules.desktop.enable = true;
  myModules.xmonad.enable = false;
  myModules.code.enable = true;
  myModules.syncthing.enable = true;
  myModules.fonts.enable = true;
  myModules.plasma.enable = true;
  myModules.nvidia.enable = true;
  myModules.gitea-runner.enable = true;
  myModules.railbird-k3s = {
    enable = true;
    serverAddr = "https://dev.railbird.ai:6443";
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/a317d456-6f84-41ee-a149-8e466e414aae";
    fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/B875-39D4";
      fsType = "vfat";
      };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/129345f3-e1e1-4d45-9db9-643160c6d564"; }
    ];

  environment.systemPackages = with pkgs; [
    android-studio
  ];

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  home-manager.users = forEachUser {
     home.stateVersion = "23.11";
  };

  system.stateVersion = "23.11";
}
