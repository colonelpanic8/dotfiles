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
  enabledModule = makeEnable config "myModules.t3codeServer" false {
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
      };

      systemd.user.services.t3code-headless = {
        Unit = {
          After = ["t3code-managed-connections.service"];
          Requires = ["t3code-managed-connections.service"];
        };
        Service = {
          Environment = ["T3CODE_ENVIRONMENT_ID=${environmentId}"];
          EnvironmentFile = "${config.xdg.configHome}/t3code/managed-access.env";
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

        startTarget = lib.mkOption {
          type = lib.types.str;
          default = "default.target";
          description = "User systemd target that starts the headless T3 Code service.";
        };
      };
    };
  }
