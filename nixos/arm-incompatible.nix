{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    discord
    google-chrome
    keybase-gui
    pommed_light
    slack
    spicetify-cli
    spotify
    tor-browser-bundle-bin
  ];
}
