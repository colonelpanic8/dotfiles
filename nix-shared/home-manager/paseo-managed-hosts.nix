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
      color = "green";
    }
    {
      label = "jimi-hendnix";
      endpoint = "jimi-hendnix:6767";
      color = "amber";
    }
    {
      label = "mac-demarco-mini";
      endpoint = "mac-demarco-mini:6767";
      color = "purple";
    }
    {
      label = "railbird-sf";
      endpoint = "railbird-sf:6767";
      color = "green";
    }
    {
      label = "ryzen-shine";
      endpoint = "ryzen-shine:6767";
      color = "blue";
    }
    {
      label = "strixi-minaj";
      endpoint = "strixi-minaj:6767";
      color = "orange";
    }
    {
      label = "alexanders-macbook-air";
      # Shared in from another tailnet, so the short MagicDNS name does not resolve.
      endpoint = "alexanders-macbook-air.tail40f15e.ts.net:6767";
      color = "red";
      # Not a fleet machine, so its daemon has its own password.
      passwordSecret = "paseo-password-alexanders-macbook-air";
    }
  ];
  fleetHostsJson = builtins.toJSON fleetHosts;
  hostPasswordSecrets = lib.unique (lib.catAttrs "passwordSecret" fleetHosts);
  configuredSecretPath = config.age.secrets.paseo-password-environment.path;
  registryPath = "${config.xdg.configHome}/paseo/managed-hosts.json";
  renderRegistry = pkgs.writeShellScript "render-paseo-managed-hosts" ''
    set -eu

    # Home Manager agenix paths intentionally contain runtime shell
    # expressions on Linux and Darwin. Let the assignment expand them.
    secret_file=${configuredSecretPath}
    ${lib.optionalString pkgs.stdenv.isDarwin ''
      /bin/wait4path "$secret_file"
    ''}
    password_line="$(${pkgs.gnugrep}/bin/grep -m1 '^PASEO_PASSWORD=' "$secret_file")"
    password="''${password_line#PASEO_PASSWORD=}"
    if [ -z "$password" ]; then
      echo "Paseo password secret is empty" >&2
      exit 1
    fi

    host_passwords='{}'
    ${lib.concatMapStrings (name: ''
        host_password_file=${config.age.secrets.${name}.path}
        ${lib.optionalString pkgs.stdenv.isDarwin ''
          /bin/wait4path "$host_password_file"
        ''}
        host_passwords="$(${pkgs.jq}/bin/jq -c \
          --arg name ${lib.escapeShellArg name} \
          --rawfile value "$host_password_file" \
          '. + {($name): ($value | rtrimstr("\n"))}' <<<"$host_passwords")"
      '')
      hostPasswordSecrets}

    registry_dir=${lib.escapeShellArg (builtins.dirOf registryPath)}
    registry_path=${lib.escapeShellArg registryPath}
    mkdir -p "$registry_dir"
    temporary="$(${pkgs.coreutils}/bin/mktemp "$registry_dir/.managed-hosts.json.XXXXXX")"
    trap 'rm -f "$temporary"' EXIT

    ${pkgs.jq}/bin/jq -n \
      --arg password "$password" \
      --argjson hostPasswords "$host_passwords" \
      --argjson hosts ${lib.escapeShellArg fleetHostsJson} \
      '{version: 1, hosts: ($hosts | map(
        if .passwordSecret
        then del(.passwordSecret) + {password: $hostPasswords[.passwordSecret]}
        else . + {password: $password}
        end))}' \
      > "$temporary"
    chmod 0600 "$temporary"
    mv "$temporary" "$registry_path"
    trap - EXIT
  '';
in {
  age.secrets =
    {
      paseo-password-environment.file = ../../nixos/secrets/paseo-password-environment.age;
    }
    // lib.genAttrs hostPasswordSecrets (name: {
      file = ../../nixos/secrets + "/${name}.age";
    });

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
    domain = "user";
    config = {
      ProgramArguments = ["${renderRegistry}"];
      RunAtLoad = true;
      StandardOutPath = "${config.home.homeDirectory}/Library/Logs/paseo-managed-hosts.log";
      StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/paseo-managed-hosts.err.log";
    };
  };
}
