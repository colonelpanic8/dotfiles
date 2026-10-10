{
  config,
  libDir,
  lib,
  pkgs,
  ...
}: let
  cfg = config.dotfiles;
  srcDotfilesDir = builtins.dirOf libDir;
  worktreeLibDir = "${cfg.worktreeDotfilesDir}/lib";
  outOfStore = config.lib.file.mkOutOfStoreSymlink;
  multiplexerAliases = import ../../nix-shared/multiplexer-aliases.nix;

  # Entries that carry Ivan's identity or agent setup rather than general
  # tooling; only linked for users that import ./personal.nix.
  personalTopLevelEntries = [
    "agents"
    "claude"
    "gitconfig.org-agenda-api"
    "pypirc"
  ];

  personalConfigEntries = [
    "keepbook"
  ];

  excludedTopLevelEntries =
    [
      "codex"
      "config"
    ]
    ++ lib.optionals (!cfg.personal) personalTopLevelEntries;

  excludedConfigEntries =
    [
      "starship.toml"
    ]
    ++ lib.optionals (!cfg.personal) personalConfigEntries;

  dotfilesLinks = lib.listToAttrs (map (name: {
    name = ".${name}";
    value = {
      force = true;
      source = outOfStore "${cfg.worktreeDotfilesDir}/${name}";
    };
  }) (lib.subtractLists excludedTopLevelEntries (builtins.attrNames (builtins.readDir srcDotfilesDir))));

  xdgConfigLinks = lib.listToAttrs (map (name: {
    name = name;
    value = {
      force = true;
      source = outOfStore "${cfg.worktreeDotfilesDir}/config/${name}";
    };
  }) (lib.subtractLists excludedConfigEntries (builtins.attrNames (builtins.readDir "${srcDotfilesDir}/config"))));
in {
  options.dotfiles = {
    checkout = lib.mkOption {
      type = lib.types.str;
      default = "/Users/Shared/dotfiles";
    };
    worktreeDotfilesDir = lib.mkOption {
      type = lib.types.str;
      default = "${cfg.checkout}/dotfiles";
      readOnly = true;
    };
    personal = lib.mkEnableOption "Ivan's personal dotfiles, secrets, and agent setup";
    gitIdentity = lib.mkOption {
      type = lib.types.nullOr (lib.types.submodule {
        options = {
          name = lib.mkOption {type = lib.types.str;};
          email = lib.mkOption {type = lib.types.str;};
        };
      });
      default = null;
      description = "Written to ~/.gitconfig.custom, which the shared gitconfig includes after its own [user].";
    };
  };

  config = {
    programs.home-manager.enable = true;

    home.file =
      dotfilesLinks
      // lib.optionalAttrs (cfg.gitIdentity != null) {
        ".gitconfig.custom".text = lib.generators.toGitINI {
          user = {inherit (cfg.gitIdentity) name email;};
        };
      };

    home.packages = with pkgs; [
      alejandra
      alt-tab-macos
      claude-code
      cocoapods
      codex
      imagemagick
      inkscape
      nodejs
      potrace
      playwright-cli
      prettier
      slack
      t3code
      tea
      typescript
      vim
      vtracer
      yarn
    ];

    home.sessionPath = [
      "$HOME/.cargo/bin"
      "${worktreeLibDir}/bin"
      "${worktreeLibDir}/functions"
    ];

    home.sessionVariables = {
      EDITOR = "emacsclient --alternate-editor emacs";
    };

    programs.ssh = {
      enable = true;
      enableDefaultConfig = false;
      settings = {
        "*" = {
          ForwardAgent = true;
          AddKeysToAgent = "no";
          Compression = false;
          ServerAliveInterval = 0;
          ServerAliveCountMax = 3;
          HashKnownHosts = false;
          UserKnownHostsFile = "~/.ssh/known_hosts";
          ControlMaster = "no";
          ControlPath = "~/.ssh/master-%r@%n:%p";
          ControlPersist = "no";
        };
      };
    };

    programs.starship = {
      enable = true;
    };

    programs.zsh = {
      enable = true;
      dotDir = "${config.home.homeDirectory}/.zsh";
      autosuggestion.enable = true;
      oh-my-zsh = {
        enable = true;
        plugins = ["git" "sudo"];
      };
      shellAliases =
        {
          df_ssh = "TERM='xterm-256color' ssh -o StrictHostKeyChecking=no";
        }
        // multiplexerAliases;
      initContent = lib.mkMerge [
        (lib.mkOrder 550 ''
          fpath+="${worktreeLibDir}/functions"
          for file in "${worktreeLibDir}/functions/"*(N); do
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

    home.stateVersion = "24.05";
  };
}
