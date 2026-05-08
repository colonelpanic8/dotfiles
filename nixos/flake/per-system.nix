{
  self,
  inputs,
  nixpkgs,
  org-agenda-api,
  agenix,
}: system: let
  pkgs = import nixpkgs {inherit system;};
  lib = pkgs.lib;

  # Get short revs for tagging
  orgApiRev = builtins.substring 0 7 (org-agenda-api.rev or "unknown");
  dotfilesRev = builtins.substring 0 7 (self.rev or self.dirtyRev or "dirty");

  # Get tangled config files from org-agenda-api.nix
  dotfilesOrgApi = import ../org-agenda-api.nix {
    inherit pkgs system;
    inherit inputs;
  };
  tangledConfig = dotfilesOrgApi.org-agenda-custom-config;

  # Import container build logic
  containerLib = import ../../org-agenda-api/container.nix {
    inherit pkgs system tangledConfig org-agenda-api orgApiRev dotfilesRev;
  };
in {
  formatter = pkgs.alejandra;

  packages =
    {
      colonelpanic-org-agenda-api = containerLib.containers.colonelpanic;
      kat-org-agenda-api = containerLib.containers.kat;
    }
    // lib.optionalAttrs pkgs.stdenv.isLinux {
      hyprNStack = inputs.hyprNStack.packages.${system}.hyprNStack;
      hyprexpo-lua = inputs.hyprland-plugins-lua.packages.${system}.hyprexpo;
      hyprwinview = inputs.hyprwinview.packages.${system}.hyprwinview;
      hypr-workspace-history = inputs.hypr-workspace-history.packages.${system}.hypr-workspace-history;
    };

  checks =
    {
      formatting =
        pkgs.runCommand "alejandra-formatting-check" {
          nativeBuildInputs = [pkgs.alejandra];
        } ''
          alejandra --check ${../.}
          touch "$out"
        '';
    }
    // lib.optionalAttrs pkgs.stdenv.isLinux {
      hyprNStack = inputs.hyprNStack.packages.${system}.hyprNStack;
      hyprexpo-lua = inputs.hyprland-plugins-lua.packages.${system}.hyprexpo;
      hyprwinview = inputs.hyprwinview.packages.${system}.hyprwinview;
      hypr-workspace-history = inputs.hypr-workspace-history.packages.${system}.hypr-workspace-history;
      hyprland-config-syntax = import ../checks/hyprland-config-syntax {
        inherit pkgs;
        hyprlandConfig = ../../dotfiles/config/hypr/hyprland.lua;
      };
      hyprland-verify-config = let
        hyprlandPackage = inputs.hyprland.packages.${system}.hyprland;
        hyprNStackPackage = inputs.hyprNStack.packages.${system}.hyprNStack;
      in
        pkgs.runCommand "hyprland-lua-verify-config" {} ''
          cp ${../../dotfiles/config/hypr/hyprland.lua} hyprland.lua
          substituteInPlace hyprland.lua \
            --replace-fail /run/current-system/sw/lib/libhyprNStack.so \
            ${hyprNStackPackage}/lib/libhyprNStack.so
          export XDG_RUNTIME_DIR="$TMPDIR/runtime"
          mkdir -p "$XDG_RUNTIME_DIR"
          HYPRLAND_NO_CRASHREPORTER=1 ${pkgs.coreutils}/bin/timeout 20s \
            ${hyprlandPackage}/bin/Hyprland --verify-config --config "$PWD/hyprland.lua"
          touch "$out"
        '';
    };

  # Dev shell for org-agenda-api deployment
  devShells.org-agenda-api = pkgs.mkShell {
    buildInputs = [
      pkgs.flyctl
      agenix.packages.${system}.default
      pkgs.age
      pkgs.ssh-to-age
      pkgs.git
      pkgs.jq
      pkgs.just
      pkgs.curl
    ];
    shellHook = ''
      echo ""
      echo "org-agenda-api deployment shell"
      echo ""
      echo "Commands:"
      echo "  just --list             - Show available API commands"
      echo "  ./deploy.sh <instance>  - Deploy to Fly.io (colonelpanic or kat)"
      echo "  flyctl                  - Fly.io CLI"
      echo "  agenix -e <file>        - Edit encrypted secrets"
      echo ""
    '';
  };
}
