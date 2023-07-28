{
  inputs = {
    nixos-hardware = { url = github:NixOS/nixos-hardware; };

    nixpkgs = { url = github:NixOS/nixpkgs/nixos-unstable; };

    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix = {
      url = github:IvanMalison/nix/my2.15.1;
    };

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.systems.follows = "systems";
    };

    systems = { url = github:nix-systems/default; };

    git-ignore-nix = {
      url = github:hercules-ci/gitignore.nix;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-wsl = { url = github:nix-community/NixOS-WSL; };
  };

  outputs = inputs@{
    self, nixpkgs, nixos-hardware, home-manager, nix, ...
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
      machinesPath = ../machines;
      machineFilenames = builtins.attrNames (builtins.readDir machinesPath);
      machineNameFromFilename = filename: builtins.head (builtins.split "\\." filename);
      mkConfigurationParams = filename: {
        name = machineNameFromFilename filename;
        value = {
          modules = [ (machinesPath + ("/" + filename)) ];
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
