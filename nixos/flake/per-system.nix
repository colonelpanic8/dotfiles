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
  hyprlandStuffPackages = import ./hyprland-stuff.nix {
    inherit pkgs inputs lib;
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
      hyprland-stuff = hyprlandStuffPackages.hyprlandStuff;
      hyprland-lua = hyprlandStuffPackages.hyprlandPackage;
      hyprNStack = hyprlandStuffPackages.hyprNStack;
      hyprexpo-lua = hyprlandStuffPackages.hyprexpo;
      hyprwinview = hyprlandStuffPackages.hyprwinview;
      hypr-workspace-history = hyprlandStuffPackages.hyprWorkspaceHistory;
      hyprtasking = hyprlandStuffPackages.hyprtasking;
      hyprglass = hyprlandStuffPackages.hyprglass;
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
      hyprNStack = hyprlandStuffPackages.hyprNStack;
      hyprexpo-lua = hyprlandStuffPackages.hyprexpo;
      hyprwinview = hyprlandStuffPackages.hyprwinview;
      hypr-workspace-history = hyprlandStuffPackages.hyprWorkspaceHistory;
      hyprtasking = hyprlandStuffPackages.hyprtasking;
      hyprglass = hyprlandStuffPackages.hyprglass;
      hyprland-config-syntax = hyprlandStuffPackages.hyprlandConfigSyntax;
      hyprland-stuff = hyprlandStuffPackages.hyprlandStuff;
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
      echo "  railbird-sf hosts production at https://org-agenda-api.rocket-sense.duckdns.org"
      echo "  ./deploy.sh <instance>  - Legacy Fly.io deploy; requires explicit opt-in"
      echo "  flyctl                  - Fly.io CLI for auditing suspended legacy apps"
      echo "  agenix -e <file>        - Edit encrypted secrets"
      echo ""
    '';
  };
}
