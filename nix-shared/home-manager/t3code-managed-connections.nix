{
  config,
  lib,
  pkgs,
  ...
}: let
  environments = import ../t3code-fleet.nix;
  keys = import ../keys.nix;
  credentialPath = config.age.secrets.t3code-managed-access-token.path;
  registryPath = "${config.xdg.configHome}/t3code/managed-connections.json";
  family = lib.optionalAttrs (config.home.username == "imalison") (import ../t3code-family.nix);
  familySecret = member: "t3code-managed-access-token.${member.hostName}";
  knownHosts = pkgs.writeText "t3code-family-known-hosts" (
    lib.concatStringsSep "\n" (lib.mapAttrsToList (_: member: "${member.sshHost} ${member.hostKey}") family) + "\n"
  );
  sshArguments = member: [
    "${pkgs.openssh}/bin/ssh"
    "-N"
    "-T"
    "-o"
    "BatchMode=yes"
    "-o"
    "ExitOnForwardFailure=yes"
    "-o"
    "ServerAliveInterval=30"
    "-o"
    "ServerAliveCountMax=3"
    "-o"
    "StrictHostKeyChecking=yes"
    "-o"
    "UserKnownHostsFile=${knownHosts}"
    "-L"
    "127.0.0.1:${toString member.localPort}:127.0.0.1:3774"
    "${member.sshUser}@${member.sshHost}"
  ];
in
  lib.mkIf (lib.elem config.home.username ["imalison" "kat"]) {
    age.secrets =
      {
        t3code-managed-access-token = {
          file = ../../nixos/secrets/t3code-managed-access-token.age;
          # agenix keeps the decrypted bytes in its runtime directory; this is a symlink.
          path = "${config.xdg.stateHome}/t3code/managed-access-token";
        };
      }
      // lib.mapAttrs' (_: member:
        lib.nameValuePair (familySecret member) {
          file = ../../nixos/secrets + "/${familySecret member}.age";
          path = "${config.xdg.stateHome}/t3code/managed-access-token.${member.hostName}";
        })
      family;

    systemd.user.services = lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux (lib.mapAttrs' (name: member:
      lib.nameValuePair "t3code-family-${name}" {
        Unit.Description = "T3 Code connection to ${member.label}'s machine";
        Service = {
          ExecStart = lib.escapeShellArgs (sshArguments member);
          Restart = "always";
          RestartSec = 5;
        };
        Install.WantedBy = ["default.target"];
      })
    family);
    launchd.agents = lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin (lib.mapAttrs' (name: member:
      lib.nameValuePair "t3code-family-${name}" {
        enable = true;
        config = {
          ProgramArguments = sshArguments member;
          RunAtLoad = true;
          KeepAlive = true;
          ThrottleInterval = 5;
          StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/t3code-family-${name}.log";
        };
      })
    family);

    home.sessionVariables.T3CODE_MANAGED_CONNECTIONS_FILE = registryPath;
    xdg.configFile."t3code/environment-groups.json".text = builtins.toJSON {
      version = 1;
      groups = import ../t3code-environment-groups.nix;
    };
    xdg.configFile."t3code/managed-connections.json" = {
      force = true;
      text = builtins.toJSON {
        version = 1;
        connections =
          map (member: member // {token."$file" = credentialPath;}) environments
          ++ lib.mapAttrsToList (_: member: {
            environmentId = "fleet:${member.hostName}";
            label = member.hostName;
            httpBaseUrl = "http://127.0.0.1:${toString member.localPort}/";
            wsBaseUrl = "ws://127.0.0.1:${toString member.localPort}/";
            token."$file" = config.age.secrets.${familySecret member}.path;
          })
          family;
      };
    };
    xdg.configFile."t3code/fleet-enrollment-recipients".text =
      lib.concatStringsSep "\n" (keys.agenixKeys ++ keys.t3codeFleetEnrollmentRecipients) + "\n";

    home.activation.removeLegacyT3FleetEnvironment = lib.hm.dag.entryAfter ["linkGeneration"] ''
      run ${pkgs.coreutils}/bin/rm -f ${lib.escapeShellArg "${config.xdg.configHome}/t3code/managed-access.env"}
      for backup in ${lib.escapeShellArg "${registryPath}.hm-backup"} ${lib.escapeShellArg "${registryPath}.hm-backup"}.*; do
        run ${pkgs.coreutils}/bin/rm -f -- "$backup"
      done
    '';
  }
