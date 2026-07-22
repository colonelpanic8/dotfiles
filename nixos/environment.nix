{
  config,
  pkgs,
  options,
  lib,
  inputs,
  ...
}: let
  environmentConfig = config.dotfilesEnvironment;
  envDotfilesWorktree = builtins.getEnv "DOTFILES_WORKTREE";
  defaultDotfilesWorktree =
    if envDotfilesWorktree != ""
    then envDotfilesWorktree
    else "/srv/dotfiles";
  zshLibDir = builtins.path {
    path = ../dotfiles/lib;
    name = "dotfiles-zsh-lib";
  };
  githubClone = pkgs.writeShellScriptBin "github_clone" ''
    export PATH="${zshLibDir}/bin:$PATH"
    exec ${pkgs.zsh}/bin/zsh ${zshLibDir}/functions/github_clone "$@"
  '';
  githubUserClone = pkgs.writeShellScriptBin "github_user_clone" ''
    export PATH="${githubClone}/bin:${zshLibDir}/bin:$PATH"
    exec ${pkgs.zsh}/bin/zsh ${zshLibDir}/functions/github_user_clone "$@"
  '';
  machineFilenames = builtins.attrNames (builtins.readDir ./machines);
  machineNameFromFilename = filename: builtins.head (builtins.split "\\." filename);
  machineNames = map machineNameFromFilename machineFilenames;
  extraManagedSshHosts = [
    "rbsf.tplinkdns.com"
  ];
  managedSshHostPatterns =
    machineNames
    ++ (map (machineName: "${machineName}.local") machineNames)
    ++ extraManagedSshHosts;
  managedSshHostCasePattern = lib.concatStringsSep "|" managedSshHostPatterns;
  multiplexerAliases = import ../nix-shared/multiplexer-aliases.nix;
in
  with lib; {
    options = {
      dotfilesEnvironment.enableHostPythonCompletions = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Enable completions supplied by the full host-specific Python
          environment. Rescue systems disable this to avoid pulling that large
          environment into otherwise lightweight installation media.
        '';
      };
      dotfiles-directory = mkOption {
        type = types.path;
        default = ../.;
      };
      dotfiles-worktree = mkOption {
        type = types.str;
        default = defaultDotfilesWorktree;
        defaultText = literalExpression ''
          builtins.getEnv "DOTFILES_WORKTREE" or "/srv/dotfiles"
        '';
        description = ''
          Runtime path to the shared, editable dotfiles checkout. Home Manager
          uses this for out-of-store symlink targets so links are stable across
          users instead of depending on each user's home directory.
        '';
      };
    };

    config = {
      environment.systemPackages = [
        pkgs.fzf
        githubClone
        githubUserClone
      ];

      # Shell configuration
      programs.zsh = {
        enable = true;
        enableCompletion = false;
        histSize = 10000000;
        setOptions = [
          "EXTENDED_HISTORY"
          "HIST_EXPIRE_DUPS_FIRST"
          "HIST_FCNTL_LOCK"
          "HIST_IGNORE_DUPS"
          "HIST_IGNORE_SPACE"
          "HIST_VERIFY"
          "SHARE_HISTORY"
        ];
        syntaxHighlighting = {
          enable = true;
        };
        ohMyZsh = {
          enable = true;
          plugins = ["git" "sudo" "pip"];
        };
        shellInit =
          ''
            # The shared editable dotfiles worktree is group-writable, which makes
            # zsh's compaudit reject our local fpath entries. Completion loading is
            # handled once by oh-my-zsh below, with this check intentionally skipped.
            ZSH_DISABLE_COMPFIX=true

            fpath=("${zshLibDir}/completions" $fpath)
            fpath+="${zshLibDir}/functions"
            for file in "${zshLibDir}/functions/"*
            do
                autoload "''${file##*/}"
            done
          ''
          + optionalString environmentConfig.enableHostPythonCompletions ''
            fpath+="${pkgs.python-with-my-packages}/lib/python3.11/site-packages/argcomplete/bash_completion.d"
          '';
        interactiveShellInit = ''
          [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"

          # Enable bracketed paste
          autoload -Uz bracketed-paste-magic
          zle -N bracketed-paste bracketed-paste-magic
        '';
        promptInit = lib.mkBefore (optionalString environmentConfig.enableHostPythonCompletions ''
          eval "$(register-python-argcomplete prb)"
          eval "$(register-python-argcomplete prod-prb)"
          eval "$(register-python-argcomplete railbird)"
        '');
      };

      programs.bash = {
        interactiveShellInit =
          optionalString environmentConfig.enableHostPythonCompletions ''
            eval "$(register-python-argcomplete prb)"
            eval "$(register-python-argcomplete prod-prb)"
            eval "$(register-python-argcomplete railbird)"
          ''
          + ''
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
        # Installing every terminfo output pulls in obsolete terminal packages
        # like termite, which can fail to build on current nixpkgs.
        enableAllTerminfo = false;
        shellAliases =
          {
            df_ssh = "TERM=xterm-256color ssh -o StrictHostKeyChecking=no";
            fix_nix = "LD_LIBRARY_PATH='' nix";
            xo = "xdg-open";
          }
          // multiplexerAliases;
        variables = {
          DOTFILES_WORKTREE = config.dotfiles-worktree;
          ROFI_SYSTEMD_TERM = "ghostty -e";
          NIXPKGS_GIT_REV = "${inputs.nixpkgs.rev}";
          NIXPKGS_SOURCE = "${inputs.nixpkgs.outPath}";
          EDITOR = "emacsclient --alternate-editor emacs";
          QT_QPA_PLATFORMTHEME = "qt6ct";
        };
        interactiveShellInit = ''
          _df_is_managed_ssh_host() {
            local host="$1"
            case "$host" in
              ${managedSshHostCasePattern}) return 0 ;;
              *) return 1 ;;
            esac
          }

          _df_ssh_target_host() {
            command ssh -G "$@" 2>/dev/null | awk '/^hostname / { print $2; exit }'
          }

          # Keep advanced TERM on managed hosts, force compatibility elsewhere.
          ssh() {
            local host
            host="$(_df_ssh_target_host "$@")"
            if [ -n "$host" ] && _df_is_managed_ssh_host "$host"; then
              command ssh "$@"
            else
              TERM=xterm-256color command ssh "$@"
            fi
          }

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
          export PATH="$HOME/.cargo/bin:${zshLibDir}/bin:${zshLibDir}/functions:$PATH";
        '';
      };
    };
  }
