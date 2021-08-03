{ config, pkgs, ... }:
{
  programs.sway.enable = true;
  services.xserver.windowManager.i3.enable = true;
  environment.systemPackages = with pkgs; [
    asciidoctor
    vscode
    gource
    gimp
    texlive.combined.scheme-full
    tor
  ];
}
