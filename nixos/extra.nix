{
  config,
  pkgs,
  makeEnable,
  ...
}:
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
    kef
    texliveSmall
    tor
    yt-dlp
  ];
}
