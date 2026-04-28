{
  inputs,
  specialArgs,
  config,
  lib,
  pkgs,
  realUsers,
  ...
}: {
  imports = [
    inputs.home-manager.nixosModules.home-manager
  ];

  options = {
    imalison.nixOverlay.enable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };
  config = {
    home-manager.users = lib.genAttrs realUsers (_: {});
    home-manager.extraSpecialArgs = {
      nixos = {
        inherit specialArgs config;
      };
    };
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.backupCommand = pkgs.writeShellScript "home-manager-backup-command" ''
      set -eu

      target_path="$1"
      backup_ext="''${HOME_MANAGER_BACKUP_EXT:-hm-backup}"
      backup_path="''${target_path}.''${backup_ext}"

      if [[ ! -e "$backup_path" ]]; then
        mv -- "$target_path" "$backup_path"
        exit 0
      fi

      timestamp="$(date +%Y%m%d-%H%M%S)"
      candidate="''${backup_path}.''${timestamp}"
      counter=0

      while [[ -e "$candidate" ]]; do
        counter=$((counter + 1))
        candidate="''${backup_path}.''${timestamp}-''${counter}"
      done

      mv -- "$target_path" "$candidate"
    '';
    home-manager.backupFileExtension = "hm-backup";
    home-manager.sharedModules = [./home-manager.nix];

    nix = rec {
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
      registry.nixpkgs.flake = inputs.nixpkgs;
      settings = {
        # Allow per-repo flake `nixConfig` (substituters, keys, etc).
        accept-flake-config = true;
        keep-outputs = true;
        keep-derivations = true;
        substituters = [
          "https://cache.nixos.org"
          "https://cuda-maintainers.cachix.org"
          "https://ai.cachix.org"
          "https://cache.nixos-cuda.org"
          "https://nix-community.cachix.org"
          "https://numtide.cachix.org"
          "https://colonelpanic8-dotfiles.cachix.org"
          "https://codex-cli.cachix.org"
          "https://claude-code.cachix.org"
        ];
        trusted-substituters = [
          "https://cache.nixos.org"
          "https://cuda-maintainers.cachix.org"
          "https://ai.cachix.org"
          "https://cache.nixos-cuda.org"
          "https://nix-community.cachix.org"
          "https://numtide.cachix.org"
          "https://colonelpanic8-dotfiles.cachix.org"
          "https://codex-cli.cachix.org"
          "https://claude-code.cachix.org"
        ];
        trusted-public-keys = [
          "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
          "ai.cachix.org-1:N9dzRK+alWwoKXQlnn0H6aUx0lU/mspIoz8hMvGvbbc="
          "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
          "colonelpanic8-dotfiles.cachix.org-1:O6GF3nptpeMFapX29okzO92eSWXR36zqW6ZF2C8P0eQ="
          "codex-cli.cachix.org-1:1Br3H1hHoRYG22n//cGKJOk3cQXgYobUel6O8DgSing="
          "claude-code.cachix.org-1:YeXf2aNu7UTX8Vwrze0za1WEDS+4DuI2kVeWEE4fsRk="
        ];
        nix-path = nixPath;
      };
      channel.enable = false;
      nixPath = [
        "nixpkgs=${inputs.nixpkgs.outPath}"
      ];
    };

    nixpkgs.overlays =
      [
        # (import ./nvidia-container-toolkit-overlay.nix)
        (import ./runc-overlay.nix)
        (import ./emacs-overlay.nix)
        (import ../nix-shared/overlays)
        # Use codex and claude-code from dedicated flakes with cachix
        (final: prev: {
          codex = inputs.codex-cli-nix.packages.${prev.stdenv.hostPlatform.system}.default;
          claude-code = inputs.claude-code-nix.packages.${prev.stdenv.hostPlatform.system}.default;
          git-sync-rs = inputs.git-sync-rs.packages.${prev.stdenv.hostPlatform.system}.default;
          kef = final.callPackage ./packages/kef {};
          pykefcontrol = final.python3Packages.callPackage ./packages/pykefcontrol {};
        })
      ]
      ++ (
        if config.imalison.nixOverlay.enable
        then [inputs.nix.overlays.default]
        else []
      );

    # Allow all the things
    nixpkgs.config.allowUnfree = true;
  };
}
