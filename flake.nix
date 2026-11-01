{
  description = "Standalone NixOS systems using shared dotfiles without private workstation inputs";

  inputs = {
    agenix = {
      url = "github:ryantm/agenix/847720f0e73a77ce6503b63fba6c357c742c28a7";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    codex-cli-nix.url = "github:sadjow/codex-cli-nix/f9eefb1b70318cf8f4a5925d1fd5ceb2dddb0f51";
    codex-desktop-linux.url = "github:ilysenko/codex-desktop-linux/f9b973bc03ea60fc971bd227948541fe84bce635";
    claude-desktop.url = "github:aaddrick/claude-desktop-debian/503165188dc96a166327dca2d1e8c04c5ac36c3c";
    claude-code-nix.url = "github:sadjow/claude-code-nix/4ce701aac994817823a05576fa3eb0ab06406e2f";
    t3code-integration.url = "github:colonelpanic8/t3code/9b42ff92929aaad77797487def6bfdc76df66dfb";
    dux-shell = {
      url = "github:colonelpanic8/dux-shell/assembled";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    quickshell-tray = {
      url = "github:colonelpanic8/quickshell/6f9dde3589dd14e1a66be715e2a85fb563611d08";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dank-ai-usage = {
      url = "github:alcxyz/DankAIUsage";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";
    systems.url = "github:nix-systems/default";
    # Keep Hyprland's release and separately exposed dependencies on the exact
    # revisions from the release lock. Plugins follow this input so they build
    # against the exact same Hyprland ABI.
    hyprland = {
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1&rev=0aa6745f705f2c4b0a5004eb019ef76e5bbdb426";
      inputs = {
        aquamarine.follows = "aquamarine";
        hyprutils.follows = "hyprutils";
      };
    };

    aquamarine = {
      url = "github:hyprwm/aquamarine/52ea11f06dd492f34b3cc0b39a9c805beecafcff";
      inputs = {
        hyprutils.follows = "hyprutils";
        nixpkgs.follows = "hyprland/nixpkgs";
        systems.follows = "systems";
      };
    };
    hyprcursor.follows = "hyprland/hyprcursor";
    hyprgraphics.follows = "hyprland/hyprgraphics";
    hyprlang.follows = "hyprland/hyprlang";
    hyprutils = {
      url = "github:hyprwm/hyprutils/0d13d5a624eb093b600d1b2cbe448fb613c86424";
      inputs.nixpkgs.follows = "hyprland/nixpkgs";
      inputs.systems.follows = "systems";
    };
    hyprlock = {
      url = "github:hyprwm/hyprlock/0332e40b8e56404f11cd79f859f208a4b9de8714";
      inputs = {
        hyprgraphics.follows = "hyprgraphics";
        hyprlang.follows = "hyprlang";
        hyprutils.follows = "hyprutils";
        hyprwayland-scanner.follows = "hyprland/hyprwayland-scanner";
        nixpkgs.follows = "hyprland/nixpkgs";
        systems.follows = "systems";
      };
    };
    xdg-desktop-portal-hyprland = {
      url = "github:hyprwm/xdg-desktop-portal-hyprland/0121b2e3f85c7ebf6280173f1054f7e5606262d1";
      inputs = {
        aquamarine.follows = "aquamarine";
        hyprgraphics.follows = "hyprgraphics";
        hyprtoolkit.follows = "hyprland/hyprtoolkit";
        hyprland-protocols.follows = "hyprland/hyprland-protocols";
        hyprlang.follows = "hyprlang";
        hyprutils.follows = "hyprutils";
        hyprwayland-scanner.follows = "hyprland/hyprwayland-scanner";
        nixpkgs.follows = "hyprland/nixpkgs";
        systems.follows = "systems";
      };
    };

    hyprNStack = {
      url = "github:colonelpanic8/hyprNStack?ref=codex/hyprnstack-combined-0.57";
      inputs = {
        hyprland.follows = "hyprland";
        nixpkgs.follows = "hyprland/nixpkgs";
      };
    };

    hyprexpo = {
      # Pinned: the live-preview backend (c19cc94+) installs a shouldRenderWindow
      # hook that fires on every window/popup commit and SEGVs during normal use
      # (CPopup::visible -> shouldRenderWindow). 8bafe0d is the last commit before
      # that feature.
      url = "github:colonelpanic8/hyprexpo/8bafe0dbc7c26a3269f7e0fa01e3fa3cf54a3161";
      inputs.hyprland.follows = "hyprland";
    };

    hyprwinview = {
      url = "github:colonelpanic8/hyprwinview/6fd565e79eb02dbba03b1fe2644a17d980f9b3e7";
      inputs.hyprland.follows = "hyprland";
    };

    hyprtasking = {
      # Pending upstream PR #140: Hyprland 0.57 support.
      url = "github:colonelpanic8/hyprtasking/a3bd530ddad57c1bdd5c25fe465013d14d18553d";
      inputs = {
        hyprland.follows = "hyprland";
        nixpkgs.follows = "hyprland/nixpkgs";
        systems.follows = "systems";
      };
    };

    hypr-workspace-history = {
      url = "github:colonelpanic8/hypr-workspace-history/8a12c9d95528716b9040d0fc4a8a56ef43be4862";
      inputs.hyprland.follows = "hyprland";
      inputs.nixpkgs.follows = "hyprland/nixpkgs";
    };

    hypr-dynamic-cursors = {
      url = "github:VirtCode/hypr-dynamic-cursors/2a8d0b735328d874a78ca93b2f201649f712a74a";
      inputs = {
        hyprland.follows = "hyprland";
        nixpkgs.follows = "hyprland/nixpkgs";
      };
    };

    hyprglass = {
      url = "github:colonelpanic8/hyprglass?ref=codex/hyprland-0.55-compat";
      flake = false;
    };

    hyprsaver = {
      # Packaged locally (packages/hyprsaver); upstream's committed flake.lock
      # contains stub hashes and cannot be evaluated as a flake input.
      url = "github:maravexa/hyprsaver";
      flake = false;
    };

    hyprscratch = {
      url = "github:colonelpanic8/hyprscratch/reapply-rules-on-toggle";
      inputs.nixpkgs.follows = "hyprland/nixpkgs";
    };

    # Keep the initial migration on this computer's installed NixOS revision.
    nixpkgs.url = "github:NixOS/nixpkgs/6f06d204d8f2d8761d9da78778745974a3216381";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {nixpkgs, ...}: {
    nixosConfigurations.msi-elliott = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit inputs;
        keys = import ./nix-shared/keys.nix;
        makeEnable = import ./nixos/make-enable.nix nixpkgs.lib;
      };
      modules = [./nixos/machines/msi-elliott.nix];
    };
  };
}
