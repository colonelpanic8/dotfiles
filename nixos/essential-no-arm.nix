{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    spotify
    google-chrome
    pommed_light
    tor-browser-bundle-bin
    spicetify-cli
  ];
}
