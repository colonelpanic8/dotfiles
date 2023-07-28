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

    # dhall
    haskellPackages.dhall
    haskellPackages.dhall-json

    # misc
    perf-tools
    protobuf
  ];
}
