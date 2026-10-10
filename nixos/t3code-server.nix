{
  config,
  inputs,
  lib,
  makeEnable,
  pkgs,
  ...
}: let
  cfg = config.myModules.t3codeServer;
  environmentId = "fleet:${config.networking.hostName}";
  serverSettings = {
    providers.claudeAgent.launchArgs = "--chrome";
  };
  # The server and its settings UI own settings.json, so merge declared keys
  # into it instead of replacing it.
  ensureServerSettings = pkgs.writeShellScript "ensure-t3code-server-settings" ''
    set -eu

    settings_file="$1"
    settings_dir="$(${pkgs.coreutils}/bin/dirname "$settings_file")"
    mkdir -p "$settings_dir"
    temporary="$(${pkgs.coreutils}/bin/mktemp "$settings_dir/.settings.json.XXXXXX")"
    trap 'rm -f "$temporary"' EXIT

    desired=${lib.escapeShellArg (builtins.toJSON serverSettings)}
    if [ -f "$settings_file" ]; then
      ${pkgs.jq}/bin/jq --argjson desired "$desired" '. * $desired' "$settings_file" > "$temporary"
    else
      ${pkgs.jq}/bin/jq -n --argjson desired "$desired" '$desired' > "$temporary"
    fi

    chmod 0600 "$temporary"
    mv "$temporary" "$settings_file"
    trap - EXIT
  '';
  enabledModule = makeEnable config "myModules.t3codeServer" false {
    assertions = [
      {
        assertion = config.myModules.t3code.enable;
        message = "myModules.t3codeServer needs myModules.t3code for its managed-access token.";
      }
    ];
    users.users.imalison.linger = true;
    home-manager.sharedModules = [inputs.t3code-integration.homeManagerModules.t3code-server];
    home-manager.users.imalison = {config, ...}: {
      services.t3code = {
        enable = true;
        # The module defaults to the flake's plain `t3code` package, whose
        # home.packages entry shadows the system-level client on PATH. The
        # overlaid pkgs.t3code is the client build, whose t3code-desktop is
        # wrapped with --password-store=gnome-libsecret; without that flag
        # Electron safeStorage falls back to basic_text under Hyprland and the
        # desktop silently loses the connection catalog.
        package = pkgs.t3code;
        repositoryRoot = "/srv/dotfiles";
        tailscaleServe.port = cfg.tailscaleServePort;
        systemdTarget = cfg.startTarget;
        inherit (cfg) settings;
        keybindings = builtins.fromJSON (builtins.readFile ../nix-shared/t3code-keybindings.json);
        fleetManifest = import ../nix-shared/t3code-fleet.nix;
      };

      systemd.user.services.t3code-headless = {
        Unit = {
          After = ["t3code-managed-connections.service"];
          Requires = ["t3code-managed-connections.service"];
        };
        Service = {
          Environment = ["T3CODE_ENVIRONMENT_ID=${environmentId}"];
          EnvironmentFile = "${config.xdg.configHome}/t3code/managed-access.env";
          ExecStartPre = "${ensureServerSettings} ${lib.escapeShellArg "${config.services.t3code.dataDirectory}/userdata/settings.json"}";
        };
      };
    };
  };
in
  enabledModule
  // {
    options = lib.recursiveUpdate enabledModule.options {
      myModules.t3codeServer = {
        tailscaleServePort = lib.mkOption {
          type = lib.types.port;
          default = 443;
          description = "Tailnet-only HTTPS port exposed by Tailscale Serve.";
        };

        settings = lib.mkOption {
          type = lib.types.attrsOf lib.types.anything;
          default = {};
          description = "Server settings this host fixes for T3 Code; see services.t3code.settings.";
        };

        startTarget = lib.mkOption {
          type = lib.types.str;
          default = "default.target";
          description = "User systemd target that starts the headless T3 Code service.";
        };
      };
    };
  }
