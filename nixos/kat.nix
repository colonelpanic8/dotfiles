{ pkgs, inputs, config, makeEnable, ... }:
makeEnable config "modules.kat" false {
  environment.systemPackages = with pkgs; [
    bitwarden
    obsidian
    obs-studio
    ffmpeg
  ];

  environment.extraInit = ''
    export PAGER=cat
  '';
}
