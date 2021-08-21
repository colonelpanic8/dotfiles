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
  outputs = inputs@{
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
      (import ./overlay.nix)
    ] ++ taffybar.overlays;
    imports = [
      home-manager.nixosModule
    ];
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.users.imalison = import ./home-manager.nix;
  });
  mkConfig = args@{ system ? "x86_64-linux", baseModules ? [ forAll ], modules ? [], specialArgs ? {}, ... }:
  nixpkgs.lib.nixosSystem (args // {
    inherit system;
    modules = baseModules ++ modules;
    specialArgs = { inherit inputs; } // specialArgs;
  });
  machineFilenames = builtins.attrNames (builtins.readDir ./machines);
  machineNameFromFilename = filename: builtins.head (builtins.split "\\." filename);
  mkConfigurationParams = filename: {
    name = machineNameFromFilename filename;
    value = {
      modules = [ (./machines + ("/" + filename)) ];
    };
  };
  defaultConfigurationParams =
    builtins.listToAttrs (map mkConfigurationParams machineFilenames);
  customParams = {
    biskcomp = {
      system = "aarch64-linux";
    };
    air-gapped-pi = {
      system = "aarch64-linux";
    };
  };
  in
  {
    nixosConfigurations = builtins.mapAttrs (machineName: params:
    let machineParams =
      if builtins.hasAttr machineName customParams
      then (builtins.getAttr machineName customParams)
      else {};
    in mkConfig (params // machineParams)
    ) defaultConfigurationParams;
  };
}
