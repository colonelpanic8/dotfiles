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
  };
  outputs = { self, nixpkgs, nixos-hardware, taffybar, xmonad, xmonad-contrib }:
  let tweaks = ({...}: {
    nix.registry.nixpkgs.flake = nixpkgs;
    nixpkgs.overlays = [ taffybar.overlay xmonad.overlay xmonad-contrib.overlay ];
  });
  piHardware = ({ ... }: {
      imports = [
        nixos-hardware.nixosModules.raspberry-pi-4
      ];
  });
  in
  {
    nixosConfigurations = {
      ryzen-shine = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ tweaks ./machines/ryzen-shine.nix ];
      };
      biskcomp = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [ tweaks piHardware ./machines/biskcomp.nix ];
      };
    };
  };
}
