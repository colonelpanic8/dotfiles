{ config, pkgs, ... }:

{
  imports = [
    <nixos-hardware/raspberry-pi/4>
    ./base.nix
  ];

  hardware.raspberry-pi."4".fkms-3d.enable = true;

  boot = {
    kernelPackages = pkgs.linuxPackages_rpi4;
    tmpOnTmpfs = true;
    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
      raspberryPi = {
        enable = true;
	      version = 4;
	      firmwareConfig = ''
          dtparam=audio=on
          hdmi_drive=2
	      '';
      };
    };
  };

  hardware.enableRedistributableFirmware = true;

  networking.useDHCP = false;
  networking.interfaces.eth0.useDHCP = true;
  networking.interfaces.wlan0.useDHCP = true;

  # Enable the X11 windowing system.
  services.xserver = {
    exportConfiguration = true;
    layout = "us";
    enable = true;
    desktopManager = {
      plasma5.enable = true;
    };
    displayManager.sddm.enable = true;
  };

  powerManagement.cpuFreqGovernor = "ondemand";

  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXOS_SD";
    fsType = "ext4";
    options = [ "noatime" ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    raspberrypi-eeprom
  ];

  nix = {
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };
}
