{
  inputs = {
    nixos-hardware.url = github:nixos/nixos-hardware;
    nix = {
      url = github:IvanMalison/nix/master;
      inputs.nixpkgs.follows = "my_unstable";
    };
    my_unstable = {
      url = path:./nixpkgs;
    };
    home-manager = {
      url = path:./home-manager;
      inputs.nixpkgs.follows = "my_unstable";
    };
    xmonad-contrib = {
      url = path:../dotfiles/config/xmonad/xmonad-contrib;
      inputs.nixpkgs.follows = "my_unstable";
    };
    xmonad = {
      url = path:../dotfiles/config/xmonad/xmonad;
      inputs.nixpkgs.follows = "my_unstable";
    };
    taffybar = {
      url = path:../dotfiles/config/taffybar/taffybar;
      inputs.nixpkgs.follows = "my_unstable";
    };
  };
  outputs = {
    self, nix, my_unstable, nixos-hardware, home-manager, taffybar, xmonad,
    xmonad-contrib
  }:
  let forAll = ({ ... }: {
    nix = {
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
      registry.nixpkgs.flake = my_unstable;
    };
    nixpkgs.overlays = [
      nix.overlay taffybar.overlay xmonad.overlay xmonad-contrib.overlay
    ];
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
      ivanm-dfinity-razer = my_unstable.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ forAll ./machines/ivanm-dfinity-razer.nix ];
      };
      ryzen-shine = my_unstable.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ forAll ./machines/ryzen-shine.nix ];
      };
      biskcomp = my_unstable.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [ forAll piHardware ./machines/biskcomp.nix ];
      };
    };
  };
}
