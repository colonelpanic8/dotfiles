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
        Restart = "onfailure";
        ExecStart =
          let replace = builtins.replaceStrings [ "$XDG_RUNTIME_DIR" ] [ "\${XDG_RUNTIME_DIR}" ];
              path = replace config.age.secrets.gpg-keys.path;
              passphrasePath = replace config.age.secrets.gpg-passphrase.path;
          in "${pkgs.gnupg}/bin/gpg --pinentry-mode loopback --passphrase-file ${passphrasePath} --import ${path}";
      };
    };
  });
}
