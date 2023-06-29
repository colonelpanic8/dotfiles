{ config, pkgs, ... }:
{
  services.expressvpn.enable = true;
  programs.hyprland.enable = true;

  environment.systemPackages = with pkgs; [
    ffmpeg
    rust-analyzer
    asciidoctor
    vscode
    roomeqwizard
    razergenie
    expressvpn
    signal-desktop
    gource
    gimp
    texlive.combined.scheme-full
    tor
  ];
}
