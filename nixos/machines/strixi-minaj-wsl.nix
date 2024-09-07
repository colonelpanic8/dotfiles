{ lib, pkgs, config, inputs, forEachUser, ... }:
{
  imports = [
    ../configuration.nix
  ];

  modules.wsl.enable = true;

  networking.hostName = "strixi-minaj-wsl";

  wsl.defaultUser = "imalison";
  system.stateVersion = "23.11"; # Did you read the comment?

  home-manager.users = forEachUser {
    home.stateVersion = "23.11";
  };

  programs.gnupg = {
    agent = {
      pinentryPackage = pkgs.pinentry-curses;
      enable = true;
      enableSSHSupport = true;
    };
  };

  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.0.2u"
    "electron-12.2.3"
    "etcher"
    "electron-19.1.9"
    "openssl-1.1.1w"
    "nix-2.16.2"
  ];
}
