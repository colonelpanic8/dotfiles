{
  pkgs,
  config,
  inputs,
  lib,
  makeEnable,
  ...
}:
let
  codexDesktopLinuxSource = pkgs.applyPatches {
    name = "codex-desktop-linux-patched";
    src = inputs.codex-desktop-linux;
    patches = [ ./patches/codex-desktop-linux-gsettings-schemas.patch ];
  };
  claudeDesktopSource = inputs.claude-desktop;
  claudeDesktopNodePty = pkgs.callPackage "${claudeDesktopSource}/nix/node-pty.nix" {};
  claudeDesktop = pkgs.callPackage "${claudeDesktopSource}/nix/claude-desktop.nix" {
    node-pty = claudeDesktopNodePty;
  };
  claudeDesktopFhs = pkgs.callPackage "${claudeDesktopSource}/nix/fhs.nix" {
    claude-desktop = claudeDesktop;
  };
  codexDesktopLinux =
    let
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
in
makeEnable config "myModules.code" true {
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

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
      claudeDesktopFhs
      codex
      gemini-cli
      opencode
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
      cabal2nix
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
