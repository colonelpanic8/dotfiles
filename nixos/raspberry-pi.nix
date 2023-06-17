{ config, pkgs, inputs, ... }:

{
  imports = [
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
  ];

  hardware.raspberry-pi."4".fkms-3d.enable = true;
  hardware.raspberry-pi."4".audio.enable = true;

  hardware.enableRedistributableFirmware = true;

  networking.useDHCP = false;
  networking.interfaces.eth0.useDHCP = true;
  networking.interfaces.wlan0.useDHCP = true;

  powerManagement.cpuFreqGovernor = "ondemand";

  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXOS_SD";
    fsType = "ext4";
    options = [ "noatime" ];
  };

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
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };
}
