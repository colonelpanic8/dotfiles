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
  fleetHosts = map (member: member.label) (import ../nix-shared/t3code-fleet.nix);
  # The server keeps the links this file lists (T3CODE_MANAGED_PEER_LINKS_FILE).
  # Each peer's pairing code is its reusable credential from the fleet
  # enrollment bundle, so the file is written at start from the decrypted bundle
  # and never reaches the store. Without the bundle no file is written, and the
  # server leaves its links as they are.
  writePeerLinks = enrollment:
    pkgs.writeShellScript "t3code-write-peer-links" ''
      set -euo pipefail
      out="$1/managed-peer-links.json"
      [ -r ${lib.escapeShellArg enrollment} ] || exit 0
      umask 077
      ${pkgs.coreutils}/bin/mkdir -p "$1"
      ${pkgs.jq}/bin/jq \
        --argjson hosts ${lib.escapeShellArg (builtins.toJSON cfg.peerLinks.hosts)} \
        --arg access ${lib.escapeShellArg cfg.peerLinks.access} \
        '{version: 1, links: [.environments[] | select(.label as $host | $hosts | index($host))
          | {environmentId, label, urls: [.httpBaseUrl], access: $access, pairingCode: .credential}]}' \
        ${lib.escapeShellArg enrollment} > "$out.tmp"
      ${pkgs.coreutils}/bin/mv "$out.tmp" "$out"
    '';
  enabledModule = makeEnable config "myModules.t3codeServer" false {
    assertions = [
      {
        assertion = config.myModules.t3code.enable;
        message = "myModules.t3codeServer needs myModules.t3code for its managed-access token.";
      }
    ];
    users.users.${config.myModules.primaryUser}.linger = true;
    # The server's preview browser is a generic-linux chrome-headless-shell it
    # downloads to ~/.t3/tools and runs through nix-ld.
    programs.nix-ld = {
      enable = true;
      libraries = with pkgs; [
        alsa-lib
        at-spi2-core
        dbus
        expat
        glib
        libgbm
        libx11
        libxcb
        libxcomposite
        libxdamage
        libxext
        libxfixes
        libxkbcommon
        libxrandr
        nspr
        nss
      ];
    };
    home-manager.sharedModules = [inputs.t3code.homeManagerModules.t3code-server];
    home-manager.users.${config.myModules.primaryUser} = {config, ...}: {
      imports = [../nix-shared/home-manager/t3code-server-config.nix];

      age.secrets = lib.mkIf cfg.peerLinks.enable {
        t3code-fleet-enrollment = {
          file = ./secrets/t3code-fleet-enrollment.age;
          # The default path holds an unexpanded $XDG_RUNTIME_DIR.
          path = "${config.xdg.stateHome}/t3code/fleet-enrollment.json";
        };
      };

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
      };

      systemd.user.services.t3code-headless = {
        Unit = {
          After = ["agenix.service"];
          Requires = ["agenix.service"];
        };
        Service =
          {
            Environment =
              [
                "T3CODE_ENVIRONMENT_ID=${environmentId}"
                "T3CODE_MANAGED_ACCESS_TOKEN_FILE=${config.age.secrets.t3code-managed-access-token.path}"
              ]
              ++ lib.optional cfg.peerLinks.enable
              "T3CODE_MANAGED_PEER_LINKS_FILE=%t/t3code/managed-peer-links.json";
          }
          // lib.optionalAttrs cfg.peerLinks.enable {
            ExecStartPre = ["-${writePeerLinks config.age.secrets.t3code-fleet-enrollment.path} %t/t3code"];
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

        peerLinks = {
          enable = lib.mkEnableOption "keeping linked environments to other fleet hosts, from the fleet enrollment bundle";
          hosts = lib.mkOption {
            type = lib.types.listOf (lib.types.enum fleetHosts);
            default = lib.remove config.networking.hostName fleetHosts;
            description = "Fleet hosts this server links to.";
          };
          access = lib.mkOption {
            type = lib.types.enum ["read-only" "approval-required" "auto-accept-edits" "auto" "full-access"];
            default = "full-access";
            description = "The most this server's agents may do on each linked host.";
          };
        };

        startTarget = lib.mkOption {
          type = lib.types.str;
          default = "default.target";
          description = "User systemd target that starts the headless T3 Code service.";
        };
      };
    };
  }
