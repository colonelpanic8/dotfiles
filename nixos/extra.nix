{ config, pkgs, ... }:
{
  programs.sway.enable = true;
  services.xserver.windowManager.i3.enable = true;
  services.expressvpn.enable = true;
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
