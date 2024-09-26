{ config, pkgs, forEachUser, makeEnable, realUsers, ... }:
makeEnable config "myModules.base" true {
  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.0.2u"
    "electron-12.2.3"
    "etcher"
    "electron-19.1.9"
    "openssl-1.1.1w"
    "nix-2.16.2"
  ];

  # Disabling these waits disables the stuck on boot up issue
  systemd.services.systemd-udev-settle.enable = false;
  systemd.services.NetworkManager-wait-online.enable = false;
  systemd.services.systemd-user-sessions.enable = false;

  # Security
  programs.gnupg = {
    agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };
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
  };

  # Audio
  hardware.pulseaudio.enable = true;

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Printing
  # services.printing.enable = true;

  # Keyboard/Keymap
  console.keyMap = "us";

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  # Update timezone automatically
  services.tzupdate.enable = true;

  # TODO: Add a comment explaining what this does.
  services.locate.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.enableNvidia = true;
  hardware.nvidia-container-toolkit.enable = true;
  hardware.nvidia.open = false;

  hardware.keyboard.zsa.enable = true;

  services.logind.extraConfig = "RuntimeDirectorySize=5G";

  # For supporting gnome stuff
  services.dbus.packages = [ pkgs.gcr ];

  programs.dconf.enable = true;

  home-manager.users = forEachUser (import ./home-manager.nix);
  nix.settings.trusted-users = realUsers;
}
