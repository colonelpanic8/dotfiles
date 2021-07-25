{
  inputs = {
    nixos-hardware.url = github:IvanMalison/nixos-hardware;
    nix = {
      url = github:IvanMalison/nix/master;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs = {
      url = path:./nixpkgs;
    };
    home-manager = {
      url = path:./home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad-contrib = {
      url = path:../dotfiles/config/xmonad/xmonad-contrib;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad = {
      url = path:../dotfiles/config/xmonad/xmonad;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    taffybar = {
      url = path:../dotfiles/config/taffybar/taffybar;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    notifications-tray-icon = {
      url = github:IvanMalison/notifications-tray-icon/master;
    };
  };
  outputs = {
    self, nix, nixpkgs, nixos-hardware, home-manager, taffybar, xmonad,
    xmonad-contrib, notifications-tray-icon
  }:
  let forAll = ({ ... }: {
    nix = {
      extraOptions = ''
        experimental-features = nix-command flakes ca-references
      '';
      registry.nixpkgs.flake = nixpkgs;
    };
    nixpkgs.overlays = [
      nix.overlay xmonad.overlay xmonad-contrib.overlay
      notifications-tray-icon.overlay (import ../dotfiles/config/taffybar/overlay.nix)
    ] ++ taffybar.overlays;
    imports = [
      home-manager.nixosModule
    ];
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.users.imalison = import ./home-manager.nix;
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
      adele = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ forAll ./machines/adele.nix ({ ... }: {
          imports = [
            nixos-hardware.nixosModules.dell-xps-17-9700-intel
          ];
        })];
      };
      biskcomp = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [ forAll piHardware ./machines/biskcomp.nix ];
      };
      air-gapped-pi = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [ forAll piHardware ./machines/air-gapped-pi.nix ];
      };
    };
  };
}
