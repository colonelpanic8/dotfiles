{ config, pkgs, options, inputs, ... }:
{
  imports = [
    ./users.nix
    ./fonts.nix
    ./essential.nix
    ./environment.nix
  ];

  nixpkgs.overlays = with inputs; [
    xmonad.overlay
    xmonad-contrib.overlay
    notifications-tray-icon.overlay
    (import ../dotfiles/config/taffybar/overlay.nix)
    (import ./overlay.nix)
  ] ++ taffybar.overlays;

  # Allow all the things
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.0.2u"
  ];

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
    plugins = [ pkgs.networkmanager-l2tp ];
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
      userServices = true;
    };
  };

  # Audio
  sound.enable = true;
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
  services.gnome.at-spi2-core.enable = true;

  services.gnome.gnome-keyring.enable = true;

  services.openssh.enable = true;

  services.autorandr.enable = true;

  services.locate.enable = true;

  virtualisation.docker.enable = true;

  hardware.keyboard.zsa.enable = true;

  services.logind.extraConfig = "RuntimeDirectorySize=5G";

  services.dbus.packages = [ pkgs.gcr ];

  services.xserver = {
    exportConfiguration = true;
    enable = true;
    layout = "us";
    desktopManager = {
      plasma5.enable = true;
    };
    windowManager = {
      session = [
        {
          name = "xmonad";
          start = ''
            /usr/bin/env imalison-xmonad &
            waitPID=$!
          '';
        }
      ];
    };
    displayManager = {
      sddm = {
        enable = true;
      };
      sessionCommands = ''
        systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
      '';
    };
  };
}
