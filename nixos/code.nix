{
  pkgs,
  config,
  inputs,
  lib,
  makeEnable,
  ...
}:
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

  environment.systemPackages = with pkgs;
    [
      # LLM Tools
      antigravity
      claude-code
      codex
      inputs.codex-desktop-linux.packages.${pkgs.stdenv.hostPlatform.system}.default
      gemini-cli
      happy-coder
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
