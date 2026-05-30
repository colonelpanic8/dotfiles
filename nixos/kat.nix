{
  pkgs,
  config,
  makeEnable,
  ...
}:
makeEnable config "myModules.kat" false {
  environment.systemPackages = with pkgs; [
    obsidian
    bitwarden-desktop
    obsidian
  ];

  environment.extraInit = ''
    export PAGER=cat
  '';
}
