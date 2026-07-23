{
  config,
  lib,
  makeEnable,
  ...
}: let
  cfg = config.myModules.t3codeServer;
  enabledModule = makeEnable config "myModules.t3codeServer" false {
    home-manager.sharedModules = [../nix-shared/home-manager/t3code-server.nix];
    home-manager.users.imalison.myModules.t3codeServer = {
      enable = true;
      repositoryRoot = "/srv/dotfiles";
      inherit (cfg) tailscaleServePort;
      systemdStartTarget = cfg.startTarget;
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
          default = "graphical-session.target";
          description = "User systemd target that starts the headless T3 Code service.";
        };
      };
    };
  }
