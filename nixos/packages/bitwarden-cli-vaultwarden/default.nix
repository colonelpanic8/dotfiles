{
  fetchFromGitHub,
  stdenv,
}: let
  # Vaultwarden 1.37.3 does not implement the key-ID endpoint required by 2026.9.
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "6942ec2e593c056a1f942c68ea6f08e8dcbea7d2";
    hash = "sha256-SXX3Va/QnQap5b0UcHxeygCfKEhwxPmPbAk9HZrmvNE=";
  };
  pinnedPackages = import nixpkgs {
    inherit (stdenv.hostPlatform) system;
    config = {};
    overlays = [];
  };
in
  pinnedPackages.bitwarden-cli
