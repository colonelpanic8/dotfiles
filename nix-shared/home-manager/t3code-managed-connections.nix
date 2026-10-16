{inputs}: {
  config,
  lib,
  pkgs,
  ...
}: let
  environments = import ../t3code-fleet.nix;
  credentialPath = config.age.secrets.t3code-managed-access-token.path;
  fleetImport = import "${inputs.t3code-integration}/nix/fleet-import.nix" {
    inherit pkgs environments;
  };
  registryPath = "${config.xdg.configHome}/t3code/managed-connections.json";
in
  lib.mkIf (lib.elem config.home.username ["imalison" "kat"]) {
    age.secrets.t3code-managed-access-token = {
      file = ../../nixos/secrets/t3code-managed-access-token.age;
      # agenix keeps the decrypted bytes in its runtime directory; this is a symlink.
      path = "${config.xdg.stateHome}/t3code/managed-access-token";
    };

    home.sessionVariables.T3CODE_MANAGED_CONNECTIONS_FILE = registryPath;
    xdg.configFile."t3code/managed-connections.json" = {
      force = true;
      text = builtins.toJSON {
        version = 1;
        connections = map (member: member // {token."$file" = credentialPath;}) environments;
      };
    };
    xdg.configFile."t3code/fleet.json".source = fleetImport;

    home.activation.removeLegacyT3FleetEnvironment = lib.hm.dag.entryAfter ["linkGeneration"] ''
      run ${pkgs.coreutils}/bin/rm -f ${lib.escapeShellArg "${config.xdg.configHome}/t3code/managed-access.env"}
      for backup in ${lib.escapeShellArg "${registryPath}.hm-backup"} ${lib.escapeShellArg "${registryPath}.hm-backup"}.*; do
        run ${pkgs.coreutils}/bin/rm -f -- "$backup"
      done
    '';
  }
