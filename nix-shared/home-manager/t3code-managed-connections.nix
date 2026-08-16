{
  config,
  lib,
  pkgs,
  ...
}: let
  fleetHosts = [
    "jay-lenovo"
    "jimi-hendnix"
    "mac-demarco-mini"
    "railbird-sf"
    "ryzen-shine"
    "strixi-minaj"
  ];
  magicDnsSuffix = "taileb3aad.ts.net";
  connections =
    map (host: let
      authority = "${host}.${magicDnsSuffix}${lib.optionalString (host == "railbird-sf") ":8443"}";
    in {
      environmentId = "fleet:${host}";
      label = host;
      httpBaseUrl = "https://${authority}/";
      wsBaseUrl = "wss://${authority}/";
    })
    fleetHosts;
  connectionsJson = builtins.toJSON connections;
  configuredSecretPath = config.age.secrets.paseo-password-environment.path;
  configDirectory = "${config.xdg.configHome}/t3code";
  registryPath = "${configDirectory}/managed-connections.json";
  environmentPath = "${configDirectory}/managed-access.env";
  renderConfiguration = pkgs.writeShellScript "render-t3code-managed-connections" ''
    set -eu

    secret_file=${configuredSecretPath}
    ${lib.optionalString pkgs.stdenv.isDarwin ''
      /bin/wait4path "$secret_file"
    ''}
    password_line="$(${pkgs.gnugrep}/bin/grep -m1 '^PASEO_PASSWORD=' "$secret_file")"
    password="''${password_line#PASEO_PASSWORD=}"
    if [ -z "$password" ]; then
      echo "Fleet credential secret is empty" >&2
      exit 1
    fi

    token="$(printf 't3code-managed-access:%s' "$password" | ${pkgs.coreutils}/bin/sha256sum | ${pkgs.coreutils}/bin/cut -d' ' -f1)"
    config_dir=${lib.escapeShellArg configDirectory}
    registry_path=${lib.escapeShellArg registryPath}
    environment_path=${lib.escapeShellArg environmentPath}
    mkdir -p "$config_dir"

    registry_temporary="$(${pkgs.coreutils}/bin/mktemp "$config_dir/.managed-connections.json.XXXXXX")"
    environment_temporary="$(${pkgs.coreutils}/bin/mktemp "$config_dir/.managed-access.env.XXXXXX")"
    trap 'rm -f "$registry_temporary" "$environment_temporary"' EXIT

    ${pkgs.jq}/bin/jq -n \
      --arg token "$token" \
      --argjson connections ${lib.escapeShellArg connectionsJson} \
      '{version: 1, connections: ($connections | map(. + {token: $token}))}' \
      > "$registry_temporary"
    printf 'T3CODE_MANAGED_ACCESS_TOKEN=%s\n' "$token" > "$environment_temporary"
    chmod 0600 "$registry_temporary" "$environment_temporary"
    mv "$registry_temporary" "$registry_path"
    mv "$environment_temporary" "$environment_path"
    trap - EXIT
  '';
in
  lib.mkIf (lib.elem config.home.username ["imalison" "kat"]) {
    home.sessionVariables.T3CODE_MANAGED_CONNECTIONS_FILE = registryPath;

    systemd.user.services.t3code-managed-connections = lib.mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "Render the agenix-backed T3 Code fleet configuration";
        After = ["agenix.service"];
      };
      Install.WantedBy = ["default.target"];
      Service = {
        Type = "oneshot";
        ExecStart = "${renderConfiguration}";
      };
    };

    launchd.agents.t3code-managed-connections = lib.mkIf pkgs.stdenv.isDarwin {
      enable = true;
      config = {
        ProgramArguments = ["${renderConfiguration}"];
        RunAtLoad = true;
        StandardOutPath = "${config.home.homeDirectory}/Library/Logs/t3code-managed-connections.log";
        StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/t3code-managed-connections.err.log";
      };
    };
  }
