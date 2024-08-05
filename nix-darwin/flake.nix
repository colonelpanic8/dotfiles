{
  description = "Example Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    railbird-secrets = {
      url = "git+ssh://gitea@dev.railbird.ai:1123/railbird/secrets-flake.git";
    };
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";

    # Optional: Declarative tap management
    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };

  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, ... }:
  let
    configuration = { pkgs, config, ... }: {
      environment.systemPackages = with pkgs; [
	      emacs
        slack
        gitFull
        ripgrep
        yarn
        nodePackages.prettier
        vim
        just
        cocoapods
      ];

      nixpkgs.config.allowUnfree = true;


      # Auto upgrade nix package and the daemon service.
      services.nix-daemon.enable = true;
      launchd.user.envVariables.PATH = config.environment.systemPath;

      programs.direnv.enable = true;

      # Necessary for using flakes on this system.
      nix.settings.experimental-features = "nix-command flakes";

      # Create /etc/zshrc that loads the nix-darwin environment.
      programs.zsh.enable = true;

      # Set Git commit hash for darwin-version.
      system.configurationRevision = self.rev or self.dirtyRev or null;

      # Used for backwards compatibility, please read the changelog before changing.
      # $ darwin-rebuild changelog
      system.stateVersion = 4;

      # The platform the configuration will be used on.
      nixpkgs.hostPlatform = "aarch64-darwin";
      users.users.kat.openssh.authorizedKeys.keys = inputs.railbird-secrets.keys.kanivanKeys;
    };
  in
  {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#Kats-Mac-mini
    darwinConfigurations."Kats-Mac-mini" = nix-darwin.lib.darwinSystem {
      modules = [
        configuration
      ];
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations."Kats-Mac-mini".pkgs;
  };
}
