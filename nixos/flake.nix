
{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
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

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware = { url = "github:colonelpanic8/nixos-hardware/add-g834jzr"; };

    nixos-wsl = { url = "github:nix-community/NixOS-WSL"; };

    agenix = { url = "github:ryantm/agenix"; };

    railbird-secrets = {
      url = "git+ssh://gitea@dev.railbird.ai:1123/railbird/secrets-flake.git";
    };


    xmonad = {
      url = "github:xmonad/xmonad";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
      };
    };

    xmonad-contrib = {
      url = "github:IvanMalison/xmonad-contrib/withMyChanges";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
        xmonad.follows = "xmonad";
      };
    };

    taffybar = {
      url = "github:taffybar/taffybar/old-master";
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
      url = "path:../dotfiles/config/taffybar";
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

    vscode-server.url = "github:nix-community/nixos-vscode-server";

    nixified-ai = { url = "github:nixified-ai/flake"; };

    nixtheplanet.url = "github:matthewcroughan/nixtheplanet";
  };

  outputs = inputs@{
    self, nixpkgs, nixos-hardware, home-manager, taffybar, xmonad, nixtheplanet,
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
          (machinesFilepath + ("/" + filename)) agenix.nixosModules.default nixtheplanet.nixosModules.macos-ventura
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
        keys = (import ./keys.nix);
        usersInfo = (import ./users.nix) {
          pkgs = { zsh = "zsh"; };
          inherit keys inputs system;
        };
        realUsers = (builtins.attrNames
        (nixpkgs.lib.filterAttrs
           (_: value: (builtins.elem "isNormalUser" (builtins.attrNames value)) && value.isNormalUser) usersInfo.users.users)
        );
        mapAllKeysToValue = keys: value: builtins.listToAttrs (map (name: { inherit name value; }) keys);
        forEachUser = mapAllKeysToValue realUsers;
      } // specialArgs;
    });
  in
  {
    nixConfig = {
      substituters = [
        "https://cache.nixos.org/"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      ];
      extra-substituters = [
        "http://192.168.1.26:5050"
      ];
      extra-trusted-public-keys = [
        "1896Folsom.duckdns.org:U2FTjvP95qwAJo0oGpvmUChJCgi5zQoG1YisoI08Qoo="
      ];
    };
    nixosConfigurations = builtins.mapAttrs (machineName: params:
    let machineParams =
      if builtins.hasAttr machineName customParams
      then (builtins.getAttr machineName customParams)
      else {};
    in mkConfig (params // machineParams)
    ) defaultConfigurationParams;
  };
}
