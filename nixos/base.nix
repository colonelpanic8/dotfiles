{ config, pkgs, forEachUser, makeEnable, realUsers, ... }:
makeEnable config "myModules.base" true {
  nixpkgs.config.permittedInsecurePackages = [
    "electron-12.2.3"
    "electron-19.1.9"
    "electron-32.3.3"
    "etcher"
    "nix-2.16.2"
    "openssl-1.0.2u"
    "openssl-1.1.1w"
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
  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
  networking.networkmanager = {
    enable = true;
    enableStrongSwan = true;
    plugins = [ pkgs.networkmanager-l2tp pkgs.networkmanager-openvpn ];
  };
  networking.resolvconf.enable = false;
  services.mullvad-vpn.enable = true;

  # Audio

  services.pulseaudio.enable = true;
  services.pipewire = {
    enable = false;
    systemWide = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    jack.enable = true;
    pulse.enable = true;
  };


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

  virtualisation.podman = {
    enable = true;
    autoPrune = {
      enable = true;
    };
  };

  hardware.keyboard.zsa.enable = true;

  services.logind.extraConfig = "RuntimeDirectorySize=5G";

  # For supporting gnome stuff
  services.dbus.packages = [ pkgs.gcr ];

  programs.dconf.enable = true;

  home-manager.users = forEachUser (import ./home-manager.nix);
  nix.settings.trusted-users = realUsers ++ ["gitea-runner"];
}
