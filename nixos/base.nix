{ config, pkgs, options, ... }:
{
  imports = [
    ./users.nix
    ./fonts.nix
    ./essential.nix
  ];

  nixpkgs.overlays = [
    (import ../dotfiles/config/xmonad/overlay.nix)
    (import ../dotfiles/config/taffybar/overlay.nix)
  ];

  # Allow all the things
  nixpkgs.config.allowUnfree = true;
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
    enableStrongSwan = true;
    packages = [ pkgs.networkmanager-l2tp ];
    extraConfig = ''
      [main]
      rc-manager=resolvconf
    '';
  };

  programs.zsh.enable = true;

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

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };
  console.useXkbConfig = true;

  # Update timezone automatically
  services.tzupdate.enable = true;

  # TODO: Add a comment explaining what this does.
  services.gnome.at-spi2-core.enable = true;

  services.openssh.enable = true;

  services.autorandr.enable = true;

  services.locate.enable = true;

  virtualisation.docker.enable = true;

  hardware.keyboard.zsa.enable = true;

  services.xserver = {
    enable = true;

    xkbOptions = "ctrl:swapcaps";
    layout = "us+rwin_as_hyper";
    extraLayouts.hyper = {
      description = "Right Alt as Hyper key";
      languages = [ ];
      symbolsFile = pkgs.writeText "hyper" ''
        partial modifier_keys
        xkb_symbols "rwin_as_hyper" {
          replace key <RWIN> { [ Hyper_R ] };
          modifier_map Mod3 { <HYPR>, Hyper_L, Hyper_R };
        };

        partial modifier_keys
        xkb_symbols "home_as_hyper" {
          replace key <HOME> { [ Hyper_L ] };
          modifier_map Mod3 { <HOME>, Hyper_L, Hyper_R };
        };

        partial modifier_keys
        xkb_symbols "ralt_as_hyper" {
          replace key <RALT> { [ Hyper_L, Hyper_L ] };
	        modifier_map Mod3 { <RALT>, Hyper_L };
        };
      '';
    };

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
