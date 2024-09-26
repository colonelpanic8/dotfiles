{ pkgs, inputs, config, makeEnable, ... }:
makeEnable config "myModules.kat" false {
  environment.systemPackages = with pkgs; [
    bitwarden
    obsidian
    obs-studio
    ffmpeg
    code-cursor
  ];

  environment.extraInit = ''
    export PAGER=cat
  '';
}
