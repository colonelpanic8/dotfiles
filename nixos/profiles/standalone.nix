{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  user = config.myModules.primaryUser;
  worktree = config.dotfiles-worktree;
in {
  imports = [
    ../options.nix
    ../environment.nix
    inputs.home-manager.nixosModules.home-manager
  ];

  # The workstation entrypoint imports personal accounts, services, and secrets.
  # Standalone systems reuse the shell module with a deliberately separate profile.
  dotfilesEnvironment.enableHostPythonCompletions = false;
  dotfiles-worktree = "/srv/dotfiles";
  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.settings.trusted-users = [user];
  nix.registry.nixpkgs.flake = inputs.nixpkgs;
  nix.nixPath = ["nixpkgs=${inputs.nixpkgs}"];

  environment.systemPackages = with pkgs; [
    btop
    curl
    direnv
    fd
    gh
    git
    jq
    just
    neovim
    python3
    ripgrep
    tmux
    tree
    unzip
    zellij
  ];
  environment.variables = {
    DOTFILES_SYSTEM_FLAKE = "${worktree}#${config.networking.hostName}";
    EDITOR = lib.mkForce "nvim";
  };
  programs.direnv.enable = true;
  users.users.${user}.shell = pkgs.zsh;
  users.mutableUsers = true;
  security.sudo.extraRules = [
    {
      users = [user];
      commands = [
        {
          command = "ALL";
          options = ["NOPASSWD" "SETENV"];
        }
      ];
    }
  ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "before-dotfiles";
    sharedModules = [../git-config.nix];
    users.${user} = {config, ...}: let
      link = config.lib.file.mkOutOfStoreSymlink;
    in {
      home.username = user;
      home.homeDirectory = "/home/${user}";
      home.stateVersion = "26.05";
      programs.home-manager.enable = true;
      home.file = {
        ".tmux.conf".source = link "${worktree}/dotfiles/tmux.conf";
        ".inputrc".source = link "${worktree}/dotfiles/inputrc";
        ".gitignore".source = link "${worktree}/dotfiles/gitignore";
        ".dircolors".source = link "${worktree}/dotfiles/dircolors";
      };
      xdg.configFile."zellij/config.kdl".source = link "${worktree}/dotfiles/config/zellij/config.kdl";
      programs.git.settings.pull.rebase = false;
    };
  };

  assertions = [
    {
      assertion = lib.all (u: !u.isNormalUser || u.name == user) (builtins.attrValues config.users.users);
      message = "The standalone profile must contain only its primary human account.";
    }
    {
      assertion = lib.all (
        name:
          builtins.elem name ["tailscale-authkey" "github-cli-token"]
          && !(builtins.elem (config.myModules.tailscale.enrollment or null) [null "ivan"])
          && (name != "tailscale-authkey" || config.age.secrets.${name}.file == config.myModules.tailscale.enrollmentProfiles.${config.myModules.tailscale.enrollment}.authKeySecret)
          && lib.hasSuffix ".${config.networking.hostName}.age" (toString config.age.secrets.${name}.file)
      ) (builtins.attrNames (config.age.secrets or {}));
      message = "The standalone profile permits only its own host-scoped Tailscale and GitHub credentials, never workstation secrets.";
    }
  ];
}
