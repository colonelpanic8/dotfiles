{ inputs, specialArgs, config, lib, ... }:
{
  imports = [
    inputs.home-manager.nixosModule
  ];

  options = {
    imalison.nixOverlay.enable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };
  config = {
    home-manager.extraSpecialArgs = {
      nixos = {
        inherit specialArgs config;
      };
    };
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;

    nix = rec {
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
      registry.nixpkgs.flake = inputs.nixpkgs;
      settings = {
        keep-outputs = true;
        keep-derivations = true;
        substituters = [
          "https://cache.nixos.org"
        ];
        trusted-public-keys = [
          "cache.railbird.ai:KhnvcouxtIU2zxUcjJsm4bUK3o1S3p8xMf9qfZGF7/A="
        ];
        nix-path = nixPath;
      };
      channel.enable = false;
      nixPath = [
        "nixpkgs=${inputs.nixpkgs.outPath}"
      ];
    };

    nixpkgs.overlays = [
      (import ./overlay.nix)
    ] ++ (if config.imalison.nixOverlay.enable then [ inputs.nix.overlays.default ] else []);

    # Allow all the things
    nixpkgs.config.allowUnfree = true;
  };
}
