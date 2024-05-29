{ pkgs, inputs, config, makeEnable, ... }:
makeEnable config "modules.ben" true {
  home-manager.users.ben = {
    programs.zsh = {
      enable = true;
      shellAliases = {
        ll = "ls -alF";
        la = "ls -A";
        l = "ls -CF";
      };
    };
  };
}
