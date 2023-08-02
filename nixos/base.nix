{ config, pkgs, options, inputs, ... }:
{
  imports = [
    ./nix.nix
    ./users.nix
    ./essential.nix
    ./environment.nix
  ];

  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.0.2u"
    "electron-12.2.3"
    "etcher"
  ];

  boot.loader.systemd-boot.configurationLimit = 7;

  # Disabling these waits disables the stuck on boot up issue
  systemd.services.systemd-udev-settle.enable = false;
  systemd.services.NetworkManager-wait-online.enable = false;
  systemd.services.systemd-user-sessions.enable = false;

  # Security
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  services.pcscd.enable = true;

  # Networking
  environment.etc."ipsec.secrets".text = ''
    include ipsec.d/ipsec.nm-l2tp.secrets
  '';

  networking.firewall.enable = false;
  networking.networkmanager = {
    enable = true;
    enableStrongSwan = true;
    plugins = [ pkgs.networkmanager-l2tp pkgs.networkmanager-openvpn ];
    extraConfig = ''
      [main]
      rc-manager=resolvconf
    '';
  };

  services.avahi = {
    enable = true;
    nssmdns = true;
    publish = {
      enable = true;
      domain = true;
      workstation = true;
      userServices = true;
      addresses = true;
      hinfo = true;
    };
    extraServiceFiles = {
      ssh = "''${pkgs.avahi}/etc/avahi/services/ssh.service";
    };
  };

  # Audio
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Printing
  services.printing.enable = true;

  # Keyboard/Keymap
  console.keyMap = "us";

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  # Update timezone automatically
  services.tzupdate.enable = true;

  # TODO: Add a comment explaining what this does.
  services.gnome.at-spi2-core.enable = true;

  services.gnome.gnome-keyring.enable = true;

  services.openssh.enable = true;

  services.locate.enable = true;

  virtualisation.docker.enable = true;

  hardware.keyboard.zsa.enable = true;

  services.logind.extraConfig = "RuntimeDirectorySize=5G";

  services.dbus.packages = [ pkgs.gcr ];
}
