{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    etcher
    discord
    google-chrome
    keybase-gui
    pommed_light
    slack
    spicetify-cli
    spotify
    tor-browser-bundle-bin
    vscode
    zoom-us
  ];
}
