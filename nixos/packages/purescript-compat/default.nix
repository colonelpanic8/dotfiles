{system}: let
  source = builtins.fetchTree {
    type = "github";
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "4975466d324710c576dc11ad614684e6bd8cad8e";
    narHash = "sha256-xJ+X4hBtOcAFGBOe5nAMyMUeF9foJBmIOu3NjBqBycU=";
  };
in
  (import source.outPath {inherit system;}).purescript
