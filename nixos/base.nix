{ config, pkgs, options, ... }:
{
  imports = [
    ./users.nix
    ./fonts.nix
  ];

  nixpkgs.overlays = [
    (import ./overlays.nix)
    (import ../dotfiles/config/taffybar/taffybar/overlay.nix)
    (import ../dotfiles/config/xmonad/overlay.nix)
    (import ../dotfiles/config/taffybar/overlay.nix)
  ];

  # Allow all the things
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.android_sdk.accept_license = true;
  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.0.2u"
  ];

  # Disabling these waits disables the stuck on boot up issue
  systemd.services.systemd-udev-settle.enable = false;
  systemd.services.NetworkManager-wait-online.enable = false;
  networking.firewall.enable = false;

  # Security
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  services.pcscd.enable = true;

  # Networking
  environment.etc."ipsec.secrets".text = ''
    include ipsec.d/ipsec.nm-l2tp.secrets
  '';
  networking.networkmanager = {
    enable = true;
  };

  # Audio
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Keyboard
  console.keyMap = "us";

  # Update timezone automatically
  services.tzupdate.enable = true;

  programs.zsh.enable = true;

  # TODO: Add a comment explaining what this does.
  services.gnome.at-spi2-core.enable = true;

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
