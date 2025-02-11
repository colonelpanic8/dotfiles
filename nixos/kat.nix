{ pkgs, inputs, config, makeEnable, ... }:
makeEnable config "myModules.kat" false {
  environment.systemPackages = with pkgs; [
    bitwarden
    ffmpeg
    code-cursor
    obsidian
  ];

  environment.extraInit = ''
    export PAGER=cat
  '';
}