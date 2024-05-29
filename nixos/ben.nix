{ pkgs, inputs, config, makeEnable, ... }:
makeEnable config "modules.ben" true {
  home-manager.users.ben = {
    home-manager = {
      backupFileExtension = "backup";
    };

    programs.zsh = {
      enable = true;
      shellAliases = {
        l = "ls -CF";
        la = "ls -A";
        ll = "ls -lh";
        lla = "ls -alh";
        ls = "ls --color=auto";
        gts = "git status";
        gtl = "git log";
      };
    };
  };
}
