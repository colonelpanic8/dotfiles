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
in
  makeEnable config "myModules.code" true {
    # Code-capable hosts run the persistent backend used by the client-only
    # T3 Code desktop wrapper. Individual hosts can still opt out explicitly.
    myModules.t3codeServer.enable = lib.mkDefault true;

    # The Paseo daemon is the same kind of always-on agent backend, and the
    # desktop client below is useless without one to pair with.
    myModules.paseo.enable = lib.mkDefault true;

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
      inputs.codex-desktop-linux.homeManagerModules.default
      {
        home.sessionVariables.YDOTOOL_SOCKET = "/run/ydotoold/socket";
        systemd.user.sessionVariables.YDOTOOL_SOCKET = "/run/ydotoold/socket";
      }
    ];

    home-manager.users.imalison = lib.mkIf config.myModules.desktop.enable {
      imports = [../nix-shared/home-manager/t3code-keybindings.nix];

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
        # antigravity
        claude-code
        codex
        gemini-cli
        inputs.gmcli.packages.${pkgs.stdenv.hostPlatform.system}.default
        inputs.lastfm-edit.packages.${pkgs.stdenv.hostPlatform.system}.scrobble-scrubber-app
        opencode
        inputs.paseo.packages.${pkgs.stdenv.hostPlatform.system}.desktop
        t3code

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
        bazel
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
