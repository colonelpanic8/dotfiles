{
  imports = [
    ../internet-computer.nix
    ../raspberry-pi.nix
    ../base.nix
    # ../desktop.nix
    # ../xmonad.nix
  ];

  services.xrdp.enable = true;
  services.xrdp.defaultWindowManager = "startplasma-x11";
  networking.firewall.allowedTCPPorts = [ 3389 ];

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

  system.stateVersion = "21.05";
}
