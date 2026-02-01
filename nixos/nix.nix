{ inputs, specialArgs, config, lib, ... }:
{
  imports = [
    inputs.home-manager.nixosModules.home-manager
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
    home-manager.backupFileExtension = "backup";
    home-manager.sharedModules = [ ./home-manager.nix ];

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
          "https://cuda-maintainers.cachix.org"
          "https://ai.cachix.org"
        ];
        trusted-public-keys = [
          "cache.railbird.ai:KhnvcouxtIU2zxUcjJsm4bUK3o1S3p8xMf9qfZGF7/A="
          "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
          "ai.cachix.org-1:N9dzRK+alWwoKXQlnn0H6aUx0lU/mspIoz8hMvGvbbc="
        ];
        nix-path = nixPath;
      };
      channel.enable = false;
      nixPath = [
        "nixpkgs=${inputs.nixpkgs.outPath}"
      ];
    };

    nixpkgs.overlays = [
      # (import ./nvidia-container-toolkit-overlay.nix)
      (import ./runc-overlay.nix)
      (import ./overlay.nix)
    ] ++ (if config.imalison.nixOverlay.enable then [ inputs.nix.overlays.default ] else []);

    # Allow all the things
    nixpkgs.config.allowUnfree = true;
  };
}
