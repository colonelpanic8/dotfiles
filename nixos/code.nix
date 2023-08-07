{ pkgs, ... }:
{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  environment.systemPackages = with pkgs; [
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

    # purescript
    purescript
    spago

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
  ];
}
