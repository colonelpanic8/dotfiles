{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    spotify
    google-chrome
    pommed_light
    torbrowser
    spicetify-cli
  ];
}
