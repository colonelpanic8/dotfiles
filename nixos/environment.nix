{ config, pkgs, options, lib, inputs, ... }:
let libDir = "${config.dotfiles-directory}/dotfiles/lib";
in
with lib;
{
  options = {
    dotfiles-directory = mkOption {
      type = types.path;
      default = ../.;
    };
  };

  config = {
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
        fpath+="${pkgs.python-with-my-packages}/lib/python3.11/site-packages/argcomplete/bash_completion.d"
      '';
      interactiveShellInit = ''
        eval "$(register-python-argcomplete prb)"
        eval "$(register-python-argcomplete prod-prb)"
        eval "$(register-python-argcomplete railbird)"
        [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"

        # Enable bracketed paste
        autoload -Uz bracketed-paste-magic
        zle -N bracketed-paste bracketed-paste-magic
      '';
    };

    programs.bash = {
      interactiveShellInit = ''
        eval "$(register-python-argcomplete prb)"
        eval "$(register-python-argcomplete prod-prb)"
        eval "$(register-python-argcomplete railbird)"
        [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/bash"
      '';
    };

    programs.starship = {
      enable = true;
      interactiveOnly = true;
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
        fix_nix = "LD_LIBRARY_PATH='' nix";
        ta = "tmux attach";
      };
      variables = {
        ROFI_SYSTEMD_TERM = "ghostty -e";
        NIXPKGS_GIT_REV = "${inputs.nixpkgs.rev}";
        NIXPKGS_SOURCE = "${inputs.nixpkgs.outPath}";
        EDITOR = "emacsclient --alternate-editor emacs";
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
        export PATH="$HOME/.cargo/bin:${libDir}/bin:${libDir}/functions:$PATH";
      '';
    };
  };
}
