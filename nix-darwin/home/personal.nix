{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  replaceRuntimeDir = builtins.replaceStrings ["$XDG_RUNTIME_DIR"] ["\${XDG_RUNTIME_DIR}"];
  gpgKeyPath = replaceRuntimeDir config.age.secrets.gpg-keys.path;
  gpgPassphrasePath = replaceRuntimeDir config.age.secrets.gpg-passphrase.path;
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
in {
  imports = [
    inputs.agenix.homeManagerModules.default
    ./agents.nix
    ../../nix-shared/home-manager/codex-generated-skills.nix
    ../../nix-shared/home-manager/t3code-managed-connections.nix
    ./git-sync.nix
  ];

  dotfiles.personal = true;

  age.identityPaths = ["${config.home.homeDirectory}/.ssh/id_ed25519"];
  age.secrets.gpg-keys.file = ../../nixos/secrets/gpg-keys.age;
  age.secrets.gpg-passphrase.file = ../../nixos/secrets/gpg-passphrase.age;

  myModules.codexGeneratedSkills.enable = true;
  myModules.codexGeneratedSkills.worktreeCodexDir = "${config.dotfiles.worktreeDotfilesDir}/codex";

  home.packages = [
    (pkgs.pass.withExtensions (ext: [ext.pass-otp]))
    pkgs.gnupg
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
        if [ -n "''${XDG_RUNTIME_DIR:-}" ] && [ -r "${gpgKeyPath}" ] && [ -r "${gpgPassphrasePath}" ]; then
          ${importGpgKeyScript}
        else
          echo "Skipping GPG key import; agenix runtime secrets are not available yet" >&2
        fi
      fi
    fi
  '';

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 8 * 60 * 60;
    maxCacheTtl = 8 * 60 * 60;
    enableSshSupport = true;
    pinentry.package = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin pkgs.pinentry_mac;
    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
  };

  launchd.agents.activate-agenix = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
    domain = "user";
  };
  launchd.agents.gpg-agent = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
    domain = "user";
  };

  launchd.agents.import-gpg-key = {
    enable = true;
    domain = "user";
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

  xdg.configFile."ccusage-fleet/config.json".text = import ../../nix-shared/ccusage-fleet-config.nix {
    localHost = "mac-demarco-mini";
  };
}
