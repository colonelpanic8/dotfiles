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
          "https://taffybar.cachix.org"
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
          "https://taffybar.cachix.org"
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
          "taffybar.cachix.org-1:beZotJ1nVEsAnJxa3lWn0zwzZM7oeXmGh4ADRpHeeIo="
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
        (import ./emacs-overlay.nix)
        (import ../nix-shared/overlays)
        # Use fast-moving agent tools from dedicated flakes.
        (final: prev: let
          t3codeUnwrapped = (prev.t3code.unwrapped.override {pnpm_10 = final.pnpm_11;}).overrideAttrs (
            finalAttrs: previousAttrs: {
              version = "0.0.29-sidebar-v2-project-completion-nightly-20260720.859";
              src = inputs.t3code-sidebar-nightly;
              # Vite+ bootstraps the exact version in packageManager. Match it
              # to nixpkgs' pnpm so the task runner uses the dependency closure
              # installed offline by pnpmConfigHook.
              postPatch =
                previousAttrs.postPatch
                + ''
                  substituteInPlace package.json \
                    --replace-fail '"packageManager": "pnpm@11.10.0"' \
                                   '"packageManager": "pnpm@${final.pnpm_11.version}"'
                '';
              # The branch's Vite+ task runner checks every declared workspace
              # and tries to install the four intentionally-unfetched mobile
              # and infrastructure workspaces. Run the same desktop dependency
              # chain directly: web -> server -> Electron shell.
              buildPhase = ''
                runHook preBuild

                pushd apps/web
                ../../node_modules/.bin/vp build
                popd

                node apps/server/scripts/cli.ts build --verbose
                node apps/desktop/scripts/build-preview-annotation-css.mjs

                pushd apps/desktop
                ../../node_modules/.bin/vp pack
                popd

                runHook postBuild
              '';
              # `pnpm vp cache clean` also invokes pnpm's workspace bootstrap;
              # the build above does not enable Vite+ task caching.
              postBuild = "";
              pnpmDeps = final.fetchPnpmDeps {
                pnpm = final.pnpm_11;
                inherit
                  (finalAttrs)
                  pname
                  version
                  src
                  pnpmWorkspaces
                  ;
                fetcherVersion = 4;
                hash = "sha256-bfZDQjVdT0neQYxmNB8t+XU8mbjVsAtaTi2Vms5pzxw=";
              };
            }
          );
        in {
          codex = inputs.codex-cli-nix.packages.${prev.stdenv.hostPlatform.system}.default;
          claude-code = inputs.claude-code-nix.packages.${prev.stdenv.hostPlatform.system}.default;
          t3code = prev.t3code.override {t3code-unwrapped = t3codeUnwrapped;};
          git-sync-rs = inputs.git-sync-rs.packages.${prev.stdenv.hostPlatform.system}.default;
          agent-workspace-linux = final.callPackage ./packages/agent-workspace-linux {};
          elegant-grub2-theme = final.callPackage ./packages/elegant-grub2-theme {};
          kef = final.callPackage ./packages/kef {};
          pykefcontrol = final.python3Packages.callPackage ./packages/pykefcontrol {};
          roborock-control = final.callPackage ./packages/roborock-control {};
          rofi-roborock = final.callPackage ./packages/rofi-roborock {};
          rumno = prev.rumno.overrideAttrs (old: {
            version = "0.1.4-unstable-2026-05-26";
            src = final.fetchFromGitLab {
              owner = "ivanmalison";
              repo = "rumno";
              rev = "2049179542b75681230800bd008441b45b10ee6e";
              hash = "sha256-/sLXY5JMmCnhr6xNyDUGONiX0Ye/w7YLKD0RAgjRW8s=";
            };
          });
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
