{
  config,
  lib,
  pkgs,
  ...
}: let
  fleetHosts = [
    {
      label = "jay-lenovo";
      endpoint = "jay-lenovo:6767";
    }
    {
      label = "jimi-hendnix";
      endpoint = "jimi-hendnix:6767";
    }
    {
      label = "nixquick";
      endpoint = "nixquick:6767";
    }
    {
      label = "railbird-sf";
      endpoint = "railbird-sf:6767";
    }
    {
      label = "ryzen-shine";
      endpoint = "ryzen-shine:6767";
    }
    {
      label = "strixi-minaj";
      endpoint = "strixi-minaj:6767";
    }
  ];
  fleetHostsJson = builtins.toJSON fleetHosts;
  configuredSecretPath = config.age.secrets.paseo-password-environment.path;
  registryPath = "${config.xdg.configHome}/paseo/managed-hosts.json";
  renderRegistry = pkgs.writeShellScript "render-paseo-managed-hosts" ''
    set -eu

    # Home Manager agenix paths intentionally contain runtime shell
    # expressions on Linux and Darwin. Let the assignment expand them.
    secret_file=${configuredSecretPath}
    password_line="$(${pkgs.gnugrep}/bin/grep -m1 '^PASEO_PASSWORD=' "$secret_file")"
    password="''${password_line#PASEO_PASSWORD=}"
    if [ -z "$password" ]; then
      echo "Paseo password secret is empty" >&2
      exit 1
    fi

    registry_dir=${lib.escapeShellArg (builtins.dirOf registryPath)}
    registry_path=${lib.escapeShellArg registryPath}
    mkdir -p "$registry_dir"
    temporary="$(${pkgs.coreutils}/bin/mktemp "$registry_dir/.managed-hosts.json.XXXXXX")"
    trap 'rm -f "$temporary"' EXIT

    ${pkgs.jq}/bin/jq -n \
      --arg password "$password" \
      --argjson hosts ${lib.escapeShellArg fleetHostsJson} \
      '{version: 1, hosts: ($hosts | map(. + {password: $password}))}' \
      > "$temporary"
    chmod 0600 "$temporary"
    mv "$temporary" "$registry_path"
    trap - EXIT
  '';
in
  lib.mkIf (config.home.username == "imalison") {
    age.secrets.paseo-password-environment.file = ../../nixos/secrets/paseo-password-environment.age;

    systemd.user.services.paseo-managed-hosts = lib.mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "Render the agenix-backed Paseo fleet registry";
        After = ["agenix.service"];
      };
      Install.WantedBy = ["default.target"];
      Service = {
        Type = "oneshot";
        ExecStart = "${renderRegistry}";
      };
    };

    launchd.agents.paseo-managed-hosts = lib.mkIf pkgs.stdenv.isDarwin {
      enable = true;
      config = {
        ProgramArguments = ["${renderRegistry}"];
        RunAtLoad = true;
        StandardOutPath = "${config.home.homeDirectory}/Library/Logs/paseo-managed-hosts.log";
        StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/paseo-managed-hosts.err.log";
      };
    };
  }
