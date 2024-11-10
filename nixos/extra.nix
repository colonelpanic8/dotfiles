{ config, pkgs, makeEnable, ... }:
makeEnable config "myModules.extra" false {
  programs.hyprland.enable = true;

  environment.systemPackages = with pkgs; [
    android-studio
    gradle
    ffmpeg
    asciidoctor
    roomeqwizard
    razergenie
    signal-desktop
    gource
    gimp
    texlive.combined.scheme-full
    tor
    yt-dlp
  ];
}
