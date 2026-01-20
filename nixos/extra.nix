{ config, pkgs, makeEnable, ... }:
makeEnable config "myModules.extra" false {
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
