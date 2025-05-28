{ pkgs, config, makeEnable, ... }:
makeEnable config "myModules.kat" false {
  environment.systemPackages = with pkgs; [
    obsidian
    bitwarden
    code-cursor
    obsidian
    windsurf
  ];

  environment.extraInit = ''
    export PAGER=cat
  '';
}
