{
  inputs = {
    nixos-hardware.url = github:NixOS/nixos-hardware;
    nixpkgs = {
      # url = github:IvanMalison/nixpkgs/my-unstable;
      url = path:./nixpkgs;
    };
    home-manager = {
      url = path:./home-manager;
      # url = github:IvanMalison/home-manager/master;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad-contrib = {
      url = github:IvanMalison/xmonad-contrib/withMyChanges;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad = {
      # url = github:IvanMalison/xmonad/master;
      url = path:../dotfiles/config/xmonad/xmonad;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    taffybar = {
      url = path:../dotfiles/config/taffybar/taffybar;
      # url = github:taffybar/taffybar;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    notifications-tray-icon = {
      url = github:IvanMalison/notifications-tray-icon/master;
    };
    nix = {
      url = github:IvanMalison/nix/master;
    };
  };
  outputs = inputs@{
    self, nixpkgs, nixos-hardware, home-manager, taffybar, xmonad,
    xmonad-contrib, notifications-tray-icon, nix
  }:
  let
    mkConfig =
      args@
      { system ? "x86_64-linux"
      , baseModules ? []
      , modules ? []
      , specialArgs ? {}
      , ...
      }:
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
