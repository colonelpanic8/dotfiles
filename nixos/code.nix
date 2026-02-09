{ pkgs, config, makeEnable, ... }:
makeEnable config "myModules.code" true {
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  environment.systemPackages = with pkgs; [
    # LLM Tools
    antigravity
    claude-code
    codex
    gemini-cli
    happy-coder
    opencode

    # MCP
    github-mcp-server
    gitea-mcp-server
    playwright-mcp

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
    nodePackages.npm
    nodejs
    yarn
    prettier

    # Typescript
    typescript
    nodePackages.typescript-language-server

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
  ] ++ (if pkgs.stdenv.hostPlatform.system == "x86_64-linux" then with pkgs; [
    # purescript
    purescript
    # Broken
    # spago
  ] else []);
}
