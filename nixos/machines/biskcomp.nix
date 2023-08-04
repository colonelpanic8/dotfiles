{
  imports = [
    ../internet-computer.nix
    ../raspberry-pi.nix
    ../base.nix
    ../syncthing.nix
    ../desktop.nix
    ../code.nix
    # ../xmonad.nix
  ];

  services.plex = {
    enable = true;
  };

  services.gitea = {
    enable = true;
    lfs.enable = true;
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

  networking.hostName = "biskcomp";
  system.stateVersion = "23.11";
}
