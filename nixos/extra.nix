{ config, pkgs, ... }:
{
  programs.sway.enable = true;
  services.xserver.windowManager.i3.enable = true;
  environment.systemPackages = with pkgs; [
    rust-analyzer
    asciidoctor
    vscode
    roomeqwizard
    razergenie
    signal-desktop
    gource
    gimp
    texlive.combined.scheme-full
    tor
  ];
}
