{ pkgs, config, makeEnable, ... }:
makeEnable config "myModules.code" true {
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  environment.systemPackages = with pkgs; [
    claude-code
    codex

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
  ] ++ (if pkgs.system == "x86_64-linux" then with pkgs; [
    # purescript
    purescript
    # Broken
    # spago
  ] else []);
}
