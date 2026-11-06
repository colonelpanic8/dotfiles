{
  config,
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf config.myUsers.imalison.has.full {
    home-manager.users.imalison = {config, ...}: {
      imports = [../../../nix-shared/home-manager/gpg-trust.nix];
      age.secrets.gpg-keys.file = ../../secrets/gpg-keys.age;
      age.secrets.gpg-passphrase.file = ../../secrets/gpg-passphrase.age;
      # gws reads ~/.config/gws/client_secret.json (linked below) as its OAuth
      # client. Don't point GOOGLE_WORKSPACE_CLI_CREDENTIALS_FILE at it: that
      # variable expects user credentials and gws then fails every call with 401.
      age.secrets.gws-client-secret.file = ../../secrets/gws-client-secret.json.age;

      systemd.user.services.import-gpg-key = {
        Unit = {
          Description = "Import GPG private key";
          After = ["agenix.service"];
          # 3 total retries
          StartLimitIntervalSec = 0;
          StartLimitBurst = 3;
        };

        Install.WantedBy = ["default.target"];
        Service = {
          Type = "oneshot";
          RestartSec = 5;
          Restart = "on-failure";
          ExecStart = let
            replace = builtins.replaceStrings ["$XDG_RUNTIME_DIR"] ["\${XDG_RUNTIME_DIR}"];
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

      systemd.user.services.link-gws-client-secret = {
        Unit = {
          Description = "Link gws client secret";
          After = ["agenix.service"];
        };

        Install.WantedBy = ["default.target"];
        Service = {
          Type = "oneshot";
          ExecStart = let
            replace = builtins.replaceStrings ["$XDG_RUNTIME_DIR"] ["\${XDG_RUNTIME_DIR}"];
            secretPath = replace config.age.secrets.gws-client-secret.path;
            linkScript = pkgs.writeShellScript "link-gws-client-secret" ''
              set -eu

              config_dir="${config.xdg.configHome}/gws"
              target="${secretPath}"
              link_path="$config_dir/client_secret.json"

              mkdir -p "$config_dir"
              ln -sfn "$target" "$link_path"
            '';
          in "${linkScript}";
        };
      };
    };
  };
}
