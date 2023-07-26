{ inputs, ... }:
{
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    registry.nixpkgs.flake = inputs.nixpkgs;
  };

  nixpkgs.overlays = with inputs; [
    # nix.overlays.default
    (import ./overlay.nix)
  ];

  # Allow all the things
  nixpkgs.config.allowUnfree = true;
}
