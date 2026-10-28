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
    assertions = [
      {
        assertion = config.myModules.t3code.enable;
        message = "myModules.t3codeServer needs myModules.t3code for its managed-access token.";
      }
    ];
    users.users.imalison.linger = true;
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
    home-manager.sharedModules = [inputs.t3code-integration.homeManagerModules.t3code-server];
    home-manager.users.imalison = {config, ...}: {
      imports = [../nix-shared/home-manager/t3code-server-config.nix];

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
        Service = {
          Environment = [
            "T3CODE_ENVIRONMENT_ID=${environmentId}"
            "T3CODE_MANAGED_ACCESS_TOKEN_FILE=${config.age.secrets.t3code-managed-access-token.path}"
          ];
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
