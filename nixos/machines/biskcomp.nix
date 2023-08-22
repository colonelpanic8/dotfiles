{ forEachUser, ... }: {
  imports = [
    ../configuration.nix
    ../raspberry-pi.nix
  ];

  modules.raspberry-pi.enable = true;

  modules.base.enable = true;
  modules.desktop.enable = true;
  modules.xmonad.enable = true;
  modules.extra.enable = false;
  modules.code.enable = true;
  modules.games.enable = false;
  modules.syncthing.enable = true;
  modules.fonts.enable = true;
  modules.nixified-ai.enable = false;
  modules.cache-server.enable = true;

  services.plex = {
    enable = true;
  };

  services.gitea = {
    enable = true;
    lfs.enable = true;
    settings.server = {
      DOMAIN = "1896Folsom.duckdns.org";
      SSH_PORT = 1123;
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXOS_SD";
    fsType = "ext4";
  };

  services.home-assistant = {
    enable = true;
    extraComponents = [
      # Components required to complete the onboarding
      "met"
      "radio_browser"
    ];
    config = {
      # Includes dependencies for a basic setup
      # https://www.home-assistant.io/integrations/default_config/
      default_config = {};
    };
  };

  swapDevices = [
    { device = "/swapfile"; size = 8192; } # size is in MiB
  ];

  networking.hostName = "biskcomp";
  system.stateVersion = "23.11";

  home-manager.users = forEachUser {
    home.stateVersion = "23.11";
  };
}
