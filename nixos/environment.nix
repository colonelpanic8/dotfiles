{ config, pkgs, options, lib, inputs, ... }:
with lib;
{
  options = {
    dotfiles-directory = mkOption {
      type = types.path;
      default = ../.;
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
      shellInit = ''
        fpath+="${libDir}/functions"
        for file in "${libDir}/functions/"*
        do
            autoload "''${file##*/}"
        done
      '';
    };

    programs.starship = {
      enable = true;
      settings = {
        add_newline = false;
        character = {
          success_symbol = "[➜](bold green)";
          error_symbol = "[➜](bold red)";
        };
      };
    };

    environment = {
      homeBinInPath = true;
      localBinInPath = true;
      shellAliases = {
        df_ssh = "TERM='xterm-256color ssh -o StrictHostKeyChecking=no'";
      };
      variables = {
        ROFI_SYSTEMD_TERM = "alacritty -e";
        NIXPKGS_GIT_REV = "${inputs.nixpkgs.rev}";
        NIXPKGS_SOURCE = "${inputs.nixpkgs.outPath}";
      };
      interactiveShellInit = ''
        vterm_printf(){
            if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
                # Tell tmux to pass the escape sequences through
                printf "\ePtmux;\e\e]%s\007\e\\" "$1"
            elif [ "''${TERM%%-*}" = "screen" ]; then
                # GNU screen (screen, screen-256color, screen-256color-bce)
                printf "\eP\e]%s\007\e\\" "$1"
            else
                printf "\e]%s\e\\" "$1"
            fi
        }
        if [[ "$INSIDE_EMACS" = 'vterm' ]] \
        && [[ -n ''${EMACS_VTERM_PATH} ]] \
        && [[ -f ''${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	      source ''${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
        fi
        export STARSHIP_INSIDE_EMACS="yes"
      '';
      extraInit = ''
        export NIX_PATH="nixpkgs=${inputs.nixpkgs.outPath}:$NIX_PATH";
        export PATH="$HOME/.cargo/bin:${libDir}/bin:${libDir}/functions:$PATH";
      '';
    };
  };
}
