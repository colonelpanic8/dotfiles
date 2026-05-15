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
  hyprglass = pkgs.callPackage ../packages/hyprglass {
    src = inputs.hyprglass;
    hyprland = inputs.hyprland.packages.${system}.hyprland;
    aquamarine = inputs.aquamarine.packages.${system}.aquamarine;
    hyprcursor = inputs.hyprcursor.packages.${system}.hyprcursor;
    hyprgraphics = inputs.hyprgraphics.packages.${system}.hyprgraphics;
    hyprlang = inputs.hyprlang.packages.${system}.hyprlang;
    hyprutils = inputs.hyprutils.packages.${system}.hyprutils;
  };
  hyprexpo = pkgs.callPackage ../packages/hyprexpo {
    src = inputs.hyprexpo;
    hyprland = inputs.hyprland.packages.${system}.hyprland;
    aquamarine = inputs.aquamarine.packages.${system}.aquamarine;
    hyprcursor = inputs.hyprcursor.packages.${system}.hyprcursor;
    hyprgraphics = inputs.hyprgraphics.packages.${system}.hyprgraphics;
    hyprlang = inputs.hyprlang.packages.${system}.hyprlang;
    hyprutils = inputs.hyprutils.packages.${system}.hyprutils;
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
      hyprexpo-lua = hyprexpo;
      hyprwinview = inputs.hyprwinview.packages.${system}.hyprwinview;
      hypr-workspace-history = inputs.hypr-workspace-history.packages.${system}.hypr-workspace-history;
      inherit hyprglass;
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
      hyprexpo-lua = hyprexpo;
      hyprwinview = inputs.hyprwinview.packages.${system}.hyprwinview;
      hypr-workspace-history = inputs.hypr-workspace-history.packages.${system}.hypr-workspace-history;
      inherit hyprglass;
      hyprland-config-syntax = import ../checks/hyprland-config-syntax {
        inherit pkgs;
        hyprlandConfigDir = ../../dotfiles/config/hypr;
      };
      # Hyprland 0.54 currently segfaults in --verify-config before it can
      # validate even an empty config in this environment. Keep coverage in
      # hyprland-config-syntax until the upstream verifier is usable again.
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
