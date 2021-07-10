{ config, pkgs, ... }:

{
  imports = [
    <nixos-hardware/raspberry-pi/4>
  ];

  nixpkgs.overlays = [
    (import ./overlays.nix)
  ];

  hardware.raspberry-pi."4".fkms-3d.enable = true;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.android_sdk.accept_license = true;
  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.0.2u"
  ];

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

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  hardware.enableRedistributableFirmware = true;

  networking = {
    networkmanager.enable = true;
  };

  # networking.hostName = "nixos"; # Define your hostname.

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
    displayManager.lightdm.enable = true;
  };

  powerManagement.cpuFreqGovernor = "ondemand";

  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXOS_SD";
    fsType = "ext4";
    options = [ "noatime" ];
  };

  hardware.video.hidpi.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.imalison = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    emacs
    firefox
    raspberrypi-eeprom
    transmission-gtk
    vlc
    yubikey-manager
    networkmanagerapplet
    ncdu
    jq
    rcm
    ic-keysmith
  ];

  services.openssh.enable = true;

  services.avahi = {
    enable = true;
    nssmdns = true;
    publish = {
      enable = true;
      domain = true;
      userServices = true;
    };
  };

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  services.pcscd.enable = true;
  security.sudo.wheelNeedsPassword = false;

  nix = {
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  system.stateVersion = "21.05"; # Did you read the comment?
  nix.trustedUsers = ["imalison" "kat"];
}
