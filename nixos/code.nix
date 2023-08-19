{ pkgs, config, ... }:
{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  environment.systemPackages = with pkgs; [
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

    # Typescript
    typescript
    nodePackages.typescript-language-server

    # Rust
    rustup

    # Clojure
    boot
    leiningen

    # Ruby
    ruby

    # python
    poetry
    black

    # dhall
    haskellPackages.dhall
    haskellPackages.dhall-json

    # misc
    perf-tools
    protobuf

    # nix
    nixd
    nil
  ] ++ (if pkgs.system == "x86_64-linux" then with pkgs; [
    # purescript
    purescript
    # Broken
    # spago
  ] else []);
}
