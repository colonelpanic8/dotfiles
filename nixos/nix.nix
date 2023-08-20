{ inputs, specialArgs, config, ... }:
{
  imports = [
    inputs.home-manager.nixosModule
  ];
  home-manager.extraSpecialArgs = {
    nixos = {
      inherit specialArgs config;
    };
  };
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    registry.nixpkgs.flake = inputs.nixpkgs;
    settings = {
      keep-outputs = true;
      keep-derivations = true;
    };
    channel.enable = false;
    nixPath = [
      "nixpkgs=${inputs.nixpkgs.outPath}"
      "configuration"
    ];
  };

  nixpkgs.overlays = with inputs; [
    nix.overlays.default
    (import ./overlay.nix)
  ];

  # Allow all the things
  nixpkgs.config.allowUnfree = true;
}
