{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.gitea-actions-runner;

  settingsFormat = pkgs.formats.yaml { };

  hasDockerScheme = instance:
    instance.labels == [] || any (label: hasInfix ":docker:" label) instance.labels;
  wantsContainerRuntime = any hasDockerScheme (attrValues cfg.instances);

  hasHostScheme = instance: any (label: hasSuffix ":host" label) instance.labels;

  tokenXorTokenFile = instance:
    (instance.token == null && instance.tokenFile != null) ||
    (instance.token != null && instance.tokenFile == null);

in {
  options.services.gitea-actions-runner = {
    package = mkPackageOption pkgs "gitea-actions-runner" { };

    instances = mkOption {
      default = {};
      description = "Gitea Actions Runner instances.";
      type = types.attrsOf (types.submodule {
        options = {
          enable = mkEnableOption "Gitea Actions Runner instance";

          name = mkOption {
            type = types.str;
            example = "my-runner";
            description = "The name identifying the runner instance towards the Gitea/Forgejo instance.";
          };

          url = mkOption {
            type = types.str;
            example = "https://forge.example.com";
            description = "Base URL of your Gitea/Forgejo instance.";
          };

          token = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Plain token to register at the configured Gitea/Forgejo instance.";
          };

          tokenFile = mkOption {
            type = types.nullOr (types.either types.str types.path);
            default = null;
            description = "Path to a file containing the token to register at the configured Gitea/Forgejo instance.";
          };

          labels = mkOption {
            type = types.listOf types.str;
            default = [];
            example = [ "macos:host" "x86_64:host" ];
            description = "Labels used to map jobs to their runtime environment.";
          };

          settings = mkOption {
            description = "Configuration for `act_runner daemon`.";
            type = types.submodule {
              freeformType = settingsFormat.type;
            };
            default = { };
          };

          hostPackages = mkOption {
            type = types.listOf types.package;
            default = with pkgs; [
              bash
              coreutils
              curl
              gawk
              git
              gnused
              nodejs
              wget
              openssh
            ];
            description = "List of packages available to actions when the runner is configured with a host execution label.";
          };
        };
      });
    };
  };

  config = mkIf (cfg.instances != {}) {
    assertions = [
      {
        assertion = all tokenXorTokenFile (attrValues cfg.instances);
        message = "Instances of gitea-actions-runner can have `token` or `tokenFile`, not both.";
      }
    ];

    launchd.daemons = mapAttrs' (name: instance:
      nameValuePair "gitea-runner-${name}" {
        serviceConfig = {
          ProgramArguments = [
            "${pkgs.writeShellScript "gitea-runner-start-${name}" ''
              export HOME="/var/lib/gitea-runner/${name}"
              mkdir -p "$HOME"
              cd "$HOME"
              touch run_started
              date -d > $HOME/last-run

              # Register the runner if not already registered
              if [ ! -e "$HOME/.runner" ]; then
                ${cfg.package}/bin/act_runner register --no-interactive \
                  --instance ${escapeShellArg instance.url} \
                  --token "$TOKEN" \
                  --name ${escapeShellArg instance.name} \
                  --labels ${escapeShellArg (concatStringsSep "," instance.labels)} \
                  --config ${settingsFormat.generate "config.yaml" instance.settings}
              fi

              # Start the runner
              exec ${cfg.package}/bin/act_runner daemon --config ${settingsFormat.generate "config.yaml" instance.settings}
            ''}"
          ];
          KeepAlive = true;
          RunAtLoad = true;
          WorkingDirectory = "/var/lib/gitea-runner/${name}";
          StandardOutPath = "/var/log/gitea-runner-${name}.log";
          StandardErrorPath = "/var/log/gitea-runner-${name}.error.log";
          EnvironmentVariables = {
            PATH = (lib.makeBinPath (instance.hostPackages ++ [ cfg.package ])) + ":/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin";
          } // optionalAttrs (instance.token != null) {
            TOKEN = instance.token;
          };
        } // optionalAttrs (instance.tokenFile != null) {
          EnvironmentVariables.__TokenFile = instance.tokenFile;
        };
      }
    ) cfg.instances;

    # Ensure the log directory exists
    system.activationScripts.gitea-runner-logs = ''
      mkdir -p /var/log/gitea-runner
      mkdir -p /var/lib/gitea-runner
    '';
  };
}
