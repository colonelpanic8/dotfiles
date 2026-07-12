{
  pkgs,
  config,
  inputs,
  lib,
  makeEnable,
  ...
}: let
  codexDesktopLinuxSource = pkgs.applyPatches {
    name = "codex-desktop-linux-patched";
    src = inputs.codex-desktop-linux;
    patches = [./patches/codex-desktop-linux-gsettings-schemas.patch];
  };
  claudeDesktopSource = inputs.claude-desktop;
  claudeDesktop = pkgs.callPackage "${claudeDesktopSource}/nix/claude-desktop.nix" {};
  claudeDesktopFhs = pkgs.callPackage "${claudeDesktopSource}/nix/fhs.nix" {
    claude-desktop = claudeDesktop;
  };
  codexDesktopLinux = let
    flake = import "${codexDesktopLinuxSource}/flake.nix";
    self' =
      (flake.outputs {
        self = self';
        nixpkgs = inputs.nixpkgs;
        flake-utils = inputs.flake-utils;
      })
      // {
        outPath = "${codexDesktopLinuxSource}";
        rev = inputs.codex-desktop-linux.rev or "";
        lastModified = inputs.codex-desktop-linux.lastModified or 1;
      };
  in
    self';
  codexDesktopLinuxPackage = let
    package =
      codexDesktopLinux.packages.${pkgs.stdenv.hostPlatform.system}.codex-desktop-computer-use-ui-remote-mobile-control;
  in
    package.overrideAttrs (oldAttrs: {
      src = oldAttrs.src.overrideAttrs (payloadOldAttrs: {
        installPhase = ''
          export CODEX_ENFORCE_CRITICAL_PATCHES=0
          ${payloadOldAttrs.installPhase}
        '';
      });
    });
in
  makeEnable config "myModules.code" true {
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

    programs.ydotool = lib.mkIf config.myModules.desktop.enable {
      enable = true;
      group = "input";
    };

    home-manager.sharedModules = lib.mkIf config.myModules.desktop.enable [
      codexDesktopLinux.homeManagerModules.default
      {
        home.sessionVariables.YDOTOOL_SOCKET = "/run/ydotoold/socket";
        systemd.user.sessionVariables.YDOTOOL_SOCKET = "/run/ydotoold/socket";

        xdg.configFile."codex-desktop/settings.json".text =
          (builtins.toJSON {
            "codex-linux-computer-use-ui-enabled" = true;
          })
          + "\n";
      }
    ];

    home-manager.users.imalison = lib.mkIf config.myModules.desktop.enable {
      programs.codex = {
        enable = true;
        package = pkgs.codex;
      };

      programs.codexDesktopLinux = {
        enable = true;
        package = codexDesktopLinuxPackage;
        # Bake CODEX_CLI_PATH into the launcher so Codex Desktop always finds this
        # CLI, regardless of how it is started (GUI autostart, app launcher,
        # terminal, or warm-start handoff) and without needing a re-login.
        cliPackage = pkgs.codex;
        computerUseUi.enable = true;
        remoteMobileControl.enable = true;
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
        # antigravity
        claude-code
        codex
        gemini-cli
        inputs.gmcli.packages.${pkgs.stdenv.hostPlatform.system}.default
        inputs.lastfm-edit.packages.${pkgs.stdenv.hostPlatform.system}.scrobble-scrubber-app
        opencode

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
            # purescript
            purescript
            # Broken
            # spago
          ]
        else []
      );
  }
