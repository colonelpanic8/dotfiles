{
  config,
  lib,
  options,
  pkgs,
  ...
}: let
  cfg = config.myModules.tailscale;
  selected = cfg.enable && cfg.enrollment != null;
  known = selected && builtins.hasAttr cfg.enrollment cfg.enrollmentProfiles;
  profile = cfg.enrollmentProfiles.${cfg.enrollment};
  hasAgenix = options ? age.secrets;
  encrypted = known && profile.authKeySecret != null;
  runtimePath = path:
    path != null && lib.hasPrefix "/" path && path != "/nix/store" && !lib.hasPrefix "/nix/store/" path;
in {
  imports = [./profiles.nix];

  options.myModules.tailscale = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable the Tailscale daemon. Automatic enrollment is selected separately.";
    };
    enrollment = lib.mkOption {
      type = lib.types.nullOr lib.types.nonEmptyStr;
      default = null;
      example = "alex";
      description = "Enrollment profile for this machine; null leaves authentication manual.";
    };
    enrollmentProfiles = lib.mkOption {
      default = {};
      description = "Named enrollment profiles. Only the selected profile's credential is provisioned.";
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          authKeyFile = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Absolute runtime path to a root-owned, owner-only readable auth key. Never use a Nix path or readFile.";
          };
          authKeySecret = lib.mkOption {
            type = lib.types.nullOr lib.types.path;
            default = null;
            description = "Encrypted agenix auth-key file, as an alternative to authKeyFile. Requires the platform's agenix module.";
          };
          identityPaths = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [];
            description = "Additional agenix identities used only when this encrypted profile is selected.";
          };
          operator = lib.mkOption {
            type = lib.types.nullOr lib.types.nonEmptyStr;
            default = null;
            description = "Local operator account passed to tailscale up.";
          };
          acceptDns = lib.mkOption {
            type = lib.types.bool;
            default = true;
            description = "Accept tailnet DNS during enrollment.";
          };
          advertiseTags = lib.mkOption {
            type = lib.types.listOf (lib.types.strMatching "tag:[a-zA-Z0-9][a-zA-Z0-9-]*");
            default = [];
            description = "Tags authorized by this profile's credential and tailnet policy.";
          };
          reauthenticateExpired = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Allow automatic forced reauthentication of an expired node. Requires a renewable credential for the same tailnet.";
          };
        };
      });
    };
    enrollmentEnvironment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      internal = true;
      readOnly = true;
      description = "Resolved runtime settings consumed by the platform enrollment job.";
    };
  };

  config = lib.mkMerge [
    {
      assertions =
        lib.optionals selected [
          {
            assertion = known;
            message = "Unknown Tailscale enrollment profile '${cfg.enrollment}'. Define it in myModules.tailscale.enrollmentProfiles.";
          }
        ]
        ++ lib.optionals known [
          {
            assertion = (profile.authKeyFile != null) != (profile.authKeySecret != null);
            message = "Tailscale profile '${cfg.enrollment}' must specify exactly one of authKeyFile or authKeySecret.";
          }
          {
            assertion = profile.authKeyFile == null || runtimePath profile.authKeyFile;
            message = "Tailscale authKeyFile must be an absolute runtime path outside /nix/store.";
          }
          {
            assertion = !encrypted || hasAgenix;
            message = "Tailscale profile '${cfg.enrollment}' uses authKeySecret; import agenix for this platform or use authKeyFile.";
          }
        ];
      myModules.tailscale.enrollmentEnvironment = lib.optionalAttrs known {
        TAILSCALE = lib.getExe config.services.tailscale.package;
        JQ = lib.getExe pkgs.jq;
        STAT = "${pkgs.coreutils}/bin/stat";
        DATE = "${pkgs.coreutils}/bin/date";
        AUTH_KEY_FILE =
          if encrypted && hasAgenix
          then config.age.secrets.tailscale-authkey.path
          else if profile.authKeyFile == null
          then ""
          else profile.authKeyFile;
        NODE_HOSTNAME = config.networking.hostName;
        OPERATOR =
          if profile.operator == null
          then ""
          else profile.operator;
        ACCEPT_DNS = lib.boolToString profile.acceptDns;
        ADVERTISE_TAGS = lib.concatStringsSep "," profile.advertiseTags;
        REAUTHENTICATE_EXPIRED = lib.boolToString profile.reauthenticateExpired;
      };
    }
    (lib.optionalAttrs hasAgenix {
      age.secrets = lib.mkIf encrypted {
        tailscale-authkey = {
          file = profile.authKeySecret;
          owner = "root";
          mode = "0400";
        };
      };
      age.identityPaths = lib.mkIf encrypted (lib.mkAfter profile.identityPaths);
    })
  ];
}
