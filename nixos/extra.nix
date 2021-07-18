{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    asciidoctor
    gource
    gimp
    texlive.combined.scheme-full
    tor
  ];
}
