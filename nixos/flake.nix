{
  inputs = {
    nixos-hardware = { url = "github:NixOS/nixos-hardware"; };

    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmonad-contrib = {
      url = "git+file:..?dir=/dotfiles/config/xmonad/xmonad-contrib";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
        xmonad.follows = "xmonad";
      };
    };

    xmonad = {
      url = "git+file:..?dir=/dotfiles/config/xmonad/xmonad";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
      };
    };

    taffybar = {
      url = "git+file:..?dir=dotfiles/config/taffybar/taffybar";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
        xmonad.follows = "xmonad";
        gtk-sni-tray.follows = "gtk-sni-tray";
        gtk-strut.follows = "gtk-strut";
      };
    };

    imalison-taffybar = {
      url = "git+file:..?dir=dotfiles/config/taffybar";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        xmonad.follows = "xmonad";
        taffybar.follows = "taffybar";
      };
    };

    notifications-tray-icon = {
      url = "github:IvanMalison/notifications-tray-icon";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix = {
      url = "github:IvanMalison/nix/my2.15.1";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    systems = { url = "github:nix-systems/default"; };

    git-ignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    gtk-sni-tray = {
      url = "github:taffybar/gtk-sni-tray";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
        status-notifier-item.follows = "status-notifier-item";
      };
    };

    status-notifier-item = {
      url = "github:taffybar/status-notifier-item";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
      };
    };

    gtk-strut = {
      url = "github:taffybar/gtk-strut";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
      };
    };

    nixpkgs-regression = { url = "github:NixOS/nixpkgs"; };

    nixified-ai = { url = "github:nixified-ai/flake"; };

    nixos-wsl = { url = "github:nix-community/NixOS-WSL"; };

    agenix.url = "github:ryantm/agenix";
  };

  outputs = inputs@{
    self, nixpkgs, nixos-hardware, home-manager, taffybar, xmonad,
    xmonad-contrib, notifications-tray-icon, nix, agenix, imalison-taffybar, ...
  }:
  let
    machinesFilepath = ./machines;
    machineFilenames = builtins.attrNames (builtins.readDir machinesFilepath);
    machineNameFromFilename = filename: builtins.head (builtins.split "\\." filename);
    machineNames = map machineNameFromFilename machineFilenames;
    mkConfigurationParams = filename: {
      name = machineNameFromFilename filename;
      value = {
        modules = [
          (machinesFilepath + ("/" + filename)) agenix.nixosModules.default
        ];
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
      specialArgs = rec {
        inherit inputs machineNames;
        makeEnable = (import ./make-enable.nix) nixpkgs.lib;
        mapValueToKeys = keys: value: builtins.listToAttrs (map (name: { inherit name value; }) keys);
        realUsers = [ "root" "imalison" "kat" "dean" "alex" ];
        forEachUser = mapValueToKeys realUsers;
      } // specialArgs // (import ./keys.nix);
    });
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
