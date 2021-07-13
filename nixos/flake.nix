{
  inputs = {
    nixpkgs = {
      url = github:IvanMalison/nixpkgs/my-unstable;
    };
    taffybar = {
      url = path:../dotfiles/config/taffybar/taffybar;
    };
    xmonad-contrib = {
      url = path:../dotfiles/config/xmonad/xmonad-contrib;
    };
    xmonad = {
      url = path:../dotfiles/config/xmonad/xmonad;
    };
    nixos-hardware.url = github:nixos/nixos-hardware;
    home-manager = {
      url = github:IvanMalison/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, nixos-hardware, home-manager, taffybar, xmonad, xmonad-contrib }:
  let forAll = ({...}: {
    nix.registry.nixpkgs.flake = nixpkgs;
    nixpkgs.overlays = [ taffybar.overlay xmonad.overlay xmonad-contrib.overlay ];
    imports = [
      home-manager.nixosModule
    ];
  });
  piHardware = ({ ... }: {
      imports = [
        nixos-hardware.nixosModules.raspberry-pi-4
      ];
  });
  in
  {
    nixosConfigurations = {
      ivanm-dfinity-razer = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ forAll ./machines/ivanm-dfinity-razer.nix ];
      };
      ryzen-shine = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ forAll ./machines/ryzen-shine.nix ];
      };
      biskcomp = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [ forAll piHardware ./machines/biskcomp.nix ];
      };
    };
  };
}
