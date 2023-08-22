{ inputs, pkgs, ... }: {
  home-manager.users.imalison = ({ config, ... }: {
    imports = [ inputs.agenix.homeManagerModules.default ];
    age.identityPaths = [ "${config.home.homeDirectory}/.ssh/id_ed25519" ];
    home.packages = [
      inputs.agenix.packages."${pkgs.system}".default
    ];
    age.secrets.gpg-keys.file = ./secrets/gpg-keys.age;

    systemd.user.services.import-gpg-key = {
      Unit = {
        Description = "Import GPG private key";
      };
      Install.WantedBy = [ "default.target" ];
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.gnupg}/bin/gpg --batch --import /run/user/%U/agenix/gpg-keys";
      };
    };
  });
}
