{ config, pkgs, inputs, ... }:

{
  imports = [
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
  ];
  # https://github.com/NixOS/nixos-hardware/issues/631
  # hardware.raspberry-pi."4".fkms-3d.enable = true;
  # hardware.raspberry-pi."4".audio.enable = true;

  boot = {
    initrd.availableKernelModules = [
      "usbhid"
      "usb_storage"
      "vc4"
      "pcie_brcmstb" # required for the pcie bus to work
      "reset-raspberrypi" # required for vl805 firmware to load
    ];

    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };
  };

  boot.extraModulePackages = [ ];
  boot.kernelParams = [ ];

  hardware.enableRedistributableFirmware = true;

  # networking.useDHCP = true;
  networking.interfaces.eth0.useDHCP = true;
  networking.interfaces.wlan0.useDHCP = true;

  powerManagement.cpuFreqGovernor = "ondemand";

  environment.systemPackages = with pkgs; [
    raspberrypi-eeprom
  ];

  environment = {
    # This allows alacritty to run
    extraInit = ''
      export LIBGL_ALWAYS_SOFTWARE=1
    '';
  };

  nix = {
    # auto-optimise-store = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };
}
