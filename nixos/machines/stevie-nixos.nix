{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    ../full.nix
    ../base.nix
  ];

  hardware.enableRedistributableFirmware = true;

  boot.initrd.availableKernelModules = [
    "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc"
  ];

  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = with config.boot.kernelPackages; [ ];

  services.xserver = {
    videoDrivers = [ "modesetting ""nvidia" ];
  };

  services.thermald.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.kernelPatches = [{
    name = "enable-soundwire-drivers";
    patch = null;
    extraConfig = ''
      SND_SOC_INTEL_USER_FRIENDLY_LONG_NAMES y
      SND_SOC_INTEL_SOUNDWIRE_SOF_MACH m
      SND_SOC_RT1308 m
    '';
    ignoreConfigErrors = true;
  }];

  hardware.nvidia.prime = {
    sync.enable = true;

    # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or VGA
    nvidiaBusId = "PCI:1:0:0";

    # Bus ID of the Intel GPU. You can find it using lspci, either under 3D or VGA
    intelBusId = "PCI:0:2:0";
  };

  services.xserver.screenSection = ''
    Option         "metamodes" "nvidia-auto-select +0+0 {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}"
    Option         "AllowIndirectGLXProtocol" "off"
    Option         "TripleBuffer" "on"
  '';

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.xserver.libinput.enable = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/7c999009-1ff1-42f7-a64a-3fa91fc777a8";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."cryptroot".device = "/dev/disk/by-uuid/97c1eee7-b161-4186-9c14-6b1771d49afb";

  fileSystems."/boot" =
    { device = "/dev/disk/by-label/ESP";
      fsType = "vfat";
    };


  swapDevices = [ ];

  networking.hostName = "stevie-nixos";

  nix.maxJobs = lib.mkDefault 16;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  hardware.video.hidpi.enable = true;
  services.xserver.dpi = 180;

  system.stateVersion = "21.05";
}
