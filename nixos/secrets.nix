{ inputs, pkgs, ... }: {
  home-manager.users.imalison = ({ config, ... }: {
    imports = [ inputs.agenix.homeManagerModules.default ];
    age.identityPaths = [ "${config.home.homeDirectory}/.ssh/id_ed25519" ];
    home.packages = [
      inputs.agenix.packages."${pkgs.stdenv.hostPlatform.system}".default
    ];
    age.secrets.gpg-keys.file = ./secrets/gpg-keys.age;
    age.secrets.gpg-passphrase.file = ./secrets/gpg-passphrase.age;
    systemd.user.services.import-gpg-key = {
      Unit = {
        Description = "Import GPG private key";
        After = [ "agenix.service" ];
        # 3 total retries
        StartLimitIntervalSec = 0;
        StartLimitBurst = 3;
      };

      Install.WantedBy = [ "default.target" ];
      Service = {
        Type = "oneshot";
        RestartSec = 5;
        Restart = "on-failure";
        ExecStart =
          let
            replace = builtins.replaceStrings [ "$XDG_RUNTIME_DIR" ] [ "\${XDG_RUNTIME_DIR}" ];
            path = replace config.age.secrets.gpg-keys.path;
            passphrasePath = replace config.age.secrets.gpg-passphrase.path;
            importScript = pkgs.writeShellScript "import-gpg-key" ''
              set -eu

              normalized_key_file="$(mktemp)"
              trap 'rm -f "$normalized_key_file"' EXIT

              # Some historical exports omitted the required blank line after the
              # armor header. GnuPG imports the keys but exits non-zero, which
              # leaves the unit in a failed state.
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
              ' ${path} > "$normalized_key_file"

              exec ${pkgs.gnupg}/bin/gpg \
                --batch \
                --pinentry-mode loopback \
                --passphrase-file ${passphrasePath} \
                --import "$normalized_key_file"
            '';
          in "${importScript}";
      };
    };
  });
}
