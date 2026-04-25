{
  config,
  inputs,
  libDir,
  lib,
  pkgs,
  ...
}: let
  srcDotfilesDir = builtins.dirOf libDir;
  worktreeDotfilesDir = "${config.home.homeDirectory}/dotfiles/dotfiles";
  outOfStore = config.lib.file.mkOutOfStoreSymlink;
  replaceRuntimeDir = builtins.replaceStrings ["$XDG_RUNTIME_DIR"] ["\${XDG_RUNTIME_DIR}"];
  gpgKeyPath = replaceRuntimeDir config.age.secrets.gpg-keys.path;
  gpgPassphrasePath = replaceRuntimeDir config.age.secrets.gpg-passphrase.path;
  raycastPath = lib.concatStringsSep ":" [
    "${config.home.homeDirectory}/.nix-profile/bin"
    "/run/current-system/sw/bin"
    "/opt/homebrew/bin"
    "/usr/local/bin"
    "/usr/bin"
    "/bin"
    "/usr/sbin"
    "/sbin"
  ];
  importGpgKeyScript = pkgs.writeShellScript "import-gpg-key" ''
    set -eu

    key_path=${gpgKeyPath}
    passphrase_path=${gpgPassphrasePath}

    attempts=0
    while [ "$attempts" -lt 30 ]; do
      if [ -r "$key_path" ] && [ -r "$passphrase_path" ]; then
        break
      fi
      attempts=$((attempts + 1))
      sleep 1
    done

    if [ ! -r "$key_path" ] || [ ! -r "$passphrase_path" ]; then
      echo "Timed out waiting for agenix GPG secrets" >&2
      exit 1
    fi

    normalized_key_file="$(mktemp)"
    trap 'rm -f "$normalized_key_file"' EXIT

    # Some historical exports omitted the required blank line after the
    # armor header. GnuPG imports the keys but exits non-zero in that case.
    awk '
      pending_blank {
        if ($0 != "") {
          print ""
        }
        pending_blank = 0
      }
      { print }
      /^-----BEGIN PGP PRIVATE KEY BLOCK-----$/ {
        pending_blank = 1
      }
    ' "$key_path" > "$normalized_key_file"

    exec ${pkgs.gnupg}/bin/gpg \
      --batch \
      --pinentry-mode loopback \
      --passphrase-file "$passphrase_path" \
      --import "$normalized_key_file"
  '';
  multiplexerAliases = import ../../shared/multiplexer-aliases.nix;

  excludedTopLevelEntries = [
    "config"
  ];

  excludedConfigEntries = [
    "starship.toml"
  ];

  dotfilesLinks = lib.listToAttrs (map (name: {
    name = ".${name}";
    value = {
      force = true;
      source = outOfStore "${worktreeDotfilesDir}/${name}";
    };
  }) (lib.subtractLists excludedTopLevelEntries (builtins.attrNames (builtins.readDir srcDotfilesDir))));

  xdgConfigLinks = lib.listToAttrs (map (name: {
    name = name;
    value = {
      force = true;
      source = outOfStore "${worktreeDotfilesDir}/config/${name}";
    };
  }) (lib.subtractLists excludedConfigEntries (builtins.attrNames (builtins.readDir "${srcDotfilesDir}/config"))));
in {
  imports = [
    inputs.agenix.homeManagerModules.default
    ../../nix-shared/home-manager/codex-generated-skills.nix
  ];

  programs.home-manager.enable = true;

  age.identityPaths = ["${config.home.homeDirectory}/.ssh/id_ed25519"];
  age.secrets.gpg-keys.file = ../../nixos/secrets/gpg-keys.age;
  age.secrets.gpg-passphrase.file = ../../nixos/secrets/gpg-passphrase.age;

  home.file = dotfilesLinks;

  myModules.codexGeneratedSkills.enable = true;

  home.packages = [
    pkgs.gnupg
    (pkgs.pass.withExtensions (ext: [ext.pass-otp]))
  ];

  home.activation.repairGpgHomeAndImportKey = lib.hm.dag.entryAfter ["writeBoundary"] ''
    gnupg_dir="$HOME/.gnupg"
    password_store_gpg_id="$HOME/.password-store/.gpg-id"

    /bin/mkdir -p "$gnupg_dir"
    /bin/chmod 700 "$gnupg_dir"

    if [ -r "$password_store_gpg_id" ]; then
      needs_import=0

      while IFS= read -r recipient; do
        case "$recipient" in
          ""|\#*)
            continue
            ;;
        esac

        if ! ${pkgs.gnupg}/bin/gpg --batch --list-secret-keys --with-colons "$recipient" 2>/dev/null | /usr/bin/grep -q '^sec:'; then
          needs_import=1
          break
        fi
      done < "$password_store_gpg_id"

      if [ "$needs_import" -eq 1 ]; then
        ${importGpgKeyScript}
      fi
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
    pinentry.package = lib.mkIf pkgs.stdenv.isDarwin pkgs.pinentry_mac;
    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
  };

  launchd.agents.import-gpg-key = {
    enable = true;
    config = {
      ProgramArguments = ["${importGpgKeyScript}"];
      KeepAlive = {
        Crashed = false;
        SuccessfulExit = false;
      };
      ProcessType = "Background";
      RunAtLoad = true;
      StandardOutPath = "${config.home.homeDirectory}/Library/Logs/import-gpg-key.log";
      StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/import-gpg-key.err.log";
    };
  };

  launchd.agents.raycast = lib.mkIf pkgs.stdenv.isDarwin {
    enable = true;
    config = {
      EnvironmentVariables = {
        PATH = raycastPath;
      };
      ProgramArguments = [
        "/usr/bin/open"
        "-a"
        "Raycast"
      ];
      KeepAlive = false;
      ProcessType = "Interactive";
      RunAtLoad = true;
      StandardOutPath = "${config.home.homeDirectory}/Library/Logs/raycast-launchd.log";
      StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/raycast-launchd.err.log";
    };
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
    } // multiplexerAliases;
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
