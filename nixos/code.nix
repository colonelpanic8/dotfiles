{
  pkgs,
  config,
  inputs,
  lib,
  makeEnable,
  ...
}: let
  claudeDesktopSource = inputs.claude-desktop;
  claudeDesktopBase = pkgs.callPackage "${claudeDesktopSource}/nix/claude-desktop.nix" {};
  claudeDesktop = claudeDesktopBase.overrideAttrs (oldAttrs: {
    postFixup =
      (oldAttrs.postFixup or "")
      + ''
        # Chromium does not associate Hyprland with a native password store and
        # otherwise falls back to unencrypted basic_text storage. The session's
        # PAM-unlocked GNOME Keyring implements Secret Service, so select its
        # libsecret backend explicitly for Electron safeStorage.
        wrapProgram "$out/bin/claude-desktop" \
          --add-flags "--password-store=gnome-libsecret"
      '';
  });
  claudeDesktopFhs = pkgs.callPackage "${claudeDesktopSource}/nix/fhs.nix" {
    claude-desktop = claudeDesktop;
  };
  t3codeEnabled = config.myModules.t3code.enable;
  enabledModule = makeEnable config "myModules.code" true {
    # Code-capable hosts run the persistent backend used by the client-only
    # T3 Code desktop wrapper. Individual hosts can still opt out explicitly.
    myModules.t3codeServer.enable = lib.mkDefault t3codeEnabled;

    # The Paseo daemon is the same kind of always-on agent backend, and the
    # desktop client below is useless without one to pair with.
    myModules.paseo.enable = lib.mkDefault true;

    # Managed settings apply to every Claude config dir (~/.claude-dean etc.),
    # so transcripts (our token-usage history) are never pruned.
    environment.etc."claude-code/managed-settings.d/retention.json".text =
      builtins.toJSON {cleanupPeriodDays = 100000;};

    environment.etc."claude-code/managed-settings.d/browser-cleanup.json".text = builtins.toJSON {
      hooks.Stop = [
        {
          hooks = [
            {
              type = "command";
              command = "${lib.getExe pkgs.python3} ${../dotfiles/lib/bin/agent-browser-cleanup}";
              timeout = 10;
            }
          ];
        }
      ];
    };

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    # Claude Desktop downloads its own generic-linux Claude Code build to
    # ~/.config/Claude/claude-code/<version>/claude and execs it for every
    # session. That binary is dynamically linked against /lib64/ld-linux-x86-64.so.2,
    # which on stock NixOS is stub-ld, so it dies with exit 127. The FHS variant
    # used to supply a loader inside its bwrap sandbox; running claude-desktop
    # non-FHS (so the integrated terminal can sudo) takes that away. nix-ld puts a
    # real loader back at the FHS path, for that binary and any other prebuilt
    # native tooling (MCP servers, Cowork) these apps fetch at runtime.
    programs.nix-ld.enable = true;

    hardware.uinput.enable = lib.mkIf config.myModules.desktop.enable true;

    # Let the seat user create uinput devices (computer-use-linux's absolute
    # pointer). uaccess must be tagged before 73-seat-late.rules, so this can't
    # go in extraRules.
    services.udev.packages = lib.mkIf config.myModules.desktop.enable [
      (pkgs.writeTextDir "lib/udev/rules.d/70-uinput-uaccess.rules" ''
        KERNEL=="uinput", SUBSYSTEM=="misc", TAG+="uaccess", OPTIONS+="static_node=uinput"
      '')
    ];

    programs.ydotool = lib.mkIf config.myModules.desktop.enable {
      enable = true;
      group = "input";
    };

    home-manager.sharedModules = lib.mkIf config.myModules.desktop.enable [
      inputs.codex-desktop-linux.homeManagerModules.default
      {
        home.sessionVariables.YDOTOOL_SOCKET = "/run/ydotoold/socket";
        systemd.user.sessionVariables.YDOTOOL_SOCKET = "/run/ydotoold/socket";
      }
    ];

    home-manager.users.imalison = lib.mkIf config.myModules.desktop.enable {
      # The option only exists when the module is imported, so the settings
      # must be gated the same way; mkIf alone still references the option.
      imports = lib.optionals t3codeEnabled [
        inputs.t3code-integration.homeManagerModules.t3code-client
        {
          programs.t3code-client = {
            enable = true;
            # The overlaid client already passes --backend-mode=client-only.
            package = pkgs.t3code;
            managedConnectionsFile = "/home/imalison/.config/t3code/managed-connections.json";
            settings.sidebarV2LargeIcons = true;
            settings.composerShortcutHints = "alt";
            settings.followUpBehavior = "steer";
            settings.chatWidth = "wide";
            settings.environmentGroups = import ../nix-shared/t3code-environment-groups.nix;
            # A client-only desktop has no primary environment to take
            # keybindings from, so it needs its own copy of the managed set.
            keybindings = builtins.fromJSON (builtins.readFile ../nix-shared/t3code-keybindings.json);
          };
          xdg.dataFile."applications/com.t3tools.T3Code.desktop" = {
            force = true;
            text = ''
              [Desktop Entry]
              Type=Application
              Name=T3 Code (Nightly)
              Exec=/etc/profiles/per-user/imalison/bin/t3code-desktop %U
              Icon=t3code
              StartupWMClass=com.t3tools.T3Code
              MimeType=x-scheme-handler/t3code;
              Categories=Development;IDE;
            '';
          };
        }
      ];

      programs.codex = {
        enable = true;
        package = pkgs.codex;
      };

      programs.codexDesktopLinux = {
        enable = true;
        cliPackage = pkgs.codex;
        computerUseUi.enable = true;
        linuxFeatures = ["shallow-repository-watches"];
        remoteControl = {
          enable = true;
          package = pkgs.codex;
          extraPackages = with pkgs; [
            bash
            coreutils
            findutils
            git
            gnugrep
            gnused
            nix
            nodejs
            openssh
            ripgrep
            zsh
          ];
          listen = "unix://";
        };
      };
    };

    environment.systemPackages = with pkgs;
      [
        # LLM Tools
        claude-code
        codex
        inputs.gmcli.packages.${pkgs.stdenv.hostPlatform.system}.default
        inputs.lastfm-edit.packages.${pkgs.stdenv.hostPlatform.system}.scrobble-scrubber-app
        opencode
        inputs.paseo.packages.${pkgs.stdenv.hostPlatform.system}.desktop
      ]
      ++ [
        # MCP
        github-mcp-server
        gitea-mcp-server
        gws
        playwright-mcp
        playwright-cli

        # C
        clang

        # Haskell
        cabal-install
        ghc
        haskellPackages.hpack
        haskellPackages.hasktags
        haskellPackages.hoogle

        # Scala
        sbt
        scala

        # Node
        nodejs
        yarn
        prettier

        # Typescript
        typescript
        typescript-language-server

        # golang
        go

        # Rust
        rustup
        cargo-sweep

        # Clojure
        boot
        leiningen

        # Ruby
        ruby

        # python
        black
        poetry
        uv

        # kotlin
        kotlin
        kotlin-language-server

        # dhall
        haskellPackages.dhall
        haskellPackages.dhall-json

        # misc
        perf-tools
        protobuf

        # nix
        nixd
        nil
        alejandra
      ]
      ++ lib.optionals (config.networking.hostName != "jay-lenovo") [
        # Non-FHS variant: runs in the host namespace (no bwrap userns), so the
        # integrated Claude Code terminal can use sudo / nixos-rebuild. The FHS
        # variant (claudeDesktopFhs) sandboxes everything in an unprivileged user
        # namespace, which makes host root impossible. The FHS layout the app's
        # downloaded native binaries expect is supplied by nix-ld above instead.
        claudeDesktop
        cabal2nix
      ]
      ++ (
        if pkgs.stdenv.hostPlatform.system == "x86_64-linux"
        then
          with pkgs; [
            (import ./packages/purescript-compat {
              system = pkgs.stdenv.hostPlatform.system;
            })
            # Broken
            # spago
          ]
        else []
      );
  };
in
  enabledModule
  // {
    options = lib.recursiveUpdate enabledModule.options {
      myModules.t3code.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Install the T3 Code client and its fleet registry, and run the server by default.";
      };
    };
  }
