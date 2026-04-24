{
  config,
  libDir,
  lib,
  pkgs,
  ...
}: let
  dotfilesDir = builtins.dirOf (toString libDir);
  outOfStore = config.lib.file.mkOutOfStoreSymlink;

  excludedTopLevelEntries = [
    "agents"
    "claude"
    "codex"
    "config"
    "emacs.d"
    "zshenv"
    "zshrc"
  ];

  excludedConfigEntries = [
    "starship.toml"
  ];

  dotfilesLinks = lib.listToAttrs (map (name: {
    name = ".${name}";
    value = {
      force = true;
      source = outOfStore "${dotfilesDir}/${name}";
    };
  }) (lib.subtractLists excludedTopLevelEntries (builtins.attrNames (builtins.readDir dotfilesDir))));

  xdgConfigLinks = lib.listToAttrs (map (name: {
    name = name;
    value = {
      force = true;
      source = outOfStore "${dotfilesDir}/config/${name}";
    };
  }) (lib.subtractLists excludedConfigEntries (builtins.attrNames (builtins.readDir "${dotfilesDir}/config"))));
in {
  programs.home-manager.enable = true;

  home.file = dotfilesLinks;

  home.activation.linkEmacsDotdir = lib.hm.dag.entryAfter ["writeBoundary"] ''
    live_emacs_dir="$HOME/dotfiles/dotfiles/emacs.d"
    target_emacs_dir="${dotfilesDir}/emacs.d"
    if [ -d "$live_emacs_dir" ]; then
      target_emacs_dir="$live_emacs_dir"
    fi
    if [ -L "$HOME/.emacs.d" ] || [ ! -e "$HOME/.emacs.d" ]; then
      rm -f "$HOME/.emacs.d"
      ln -s "$target_emacs_dir" "$HOME/.emacs.d"
    else
      echo "Skipping ~/.emacs.d relink because it is not a symlink" >&2
    fi
  '';

  home.sessionPath = [
    "$HOME/.cargo/bin"
    "${libDir}/bin"
    "${libDir}/functions"
  ];

  home.sessionVariables = {
    EDITOR = "emacsclient --alternate-editor emacs";
  };

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "*" = {
        forwardAgent = true;
        addKeysToAgent = "no";
        compression = false;
        serverAliveInterval = 0;
        serverAliveCountMax = 3;
        hashKnownHosts = false;
        userKnownHostsFile = "~/.ssh/known_hosts";
        controlMaster = "no";
        controlPath = "~/.ssh/master-%r@%n:%p";
        controlPersist = "no";
      };
    };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 8 * 60 * 60;
    maxCacheTtl = 8 * 60 * 60;
    enableSshSupport = true;
    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
  };

  programs.starship = {
    enable = true;
  };

  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    oh-my-zsh = {
      enable = true;
      plugins = ["git" "sudo"];
    };
    shellAliases = {
      df_ssh = "TERM='xterm-256color' ssh -o StrictHostKeyChecking=no";
      ta = "tmux attach";
      za = "zellij attach";
    };
    initContent = lib.mkMerge [
      (lib.mkOrder 550 ''
        fpath+="${libDir}/functions"
        for file in "${libDir}/functions/"*; do
          autoload "''${file##*/}"
        done
      '')
      ''
        [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"

        autoload -Uz bracketed-paste-magic
        zle -N bracketed-paste bracketed-paste-magic
      ''
    ];
  };

  xdg.configFile = xdgConfigLinks;
}
