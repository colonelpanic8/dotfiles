{ config, pkgs, options, lib, ... }:
with lib;
{
  options = {
    dotfiles-directory = mkOption {
      type = types.str;
      default = "/home/imalison/dotfiles";
    };
  };

  config = let libDir = "${config.dotfiles-directory}/dotfiles/lib";
  in {
    # Shell configuration
    programs.zsh = {
      enable = true;
      syntaxHighlighting = {
        enable = true;
      };
      ohMyZsh = {
        enable = true;
        plugins = [ "git" "sudo" "pip" ];
      };
      spaceship-prompt.enable = true;
      shellInit = ''
        fpath+="${libDir}/functions"
        for file in "${libDir}/functions/"*
        do
            autoload "''${file##*/}"
        done
      '';
    };

    environment = {
      homeBinInPath = true;
      localBinInPath = true;
      extraInit = ''
        export PATH="${libDir}/bin:$PATH"
        export PATH="${libDir}/functions:$PATH"
      '';
    };
  };
}
