{ inputs, pkgs, ... }:
{
  imports = [
    inputs.nixos-wsl.nixosModules.wsl
    ./ssh.nix
    ./environment.nix
    ./essential.nix
    ./fonts.nix
    ./nix.nix
    ./users.nix
    ./code.nix
  ];

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  environment.variables = {
    SHELL = "${pkgs.zsh}/bin/zsh";
  };

  wsl = {
    enable = true;
    automountPath = "/mnt";
    startMenuLaunchers = true;
    nativeSystemd = true;

    docker-native.enable = true;

    # Enable integration with Docker Desktop (needs to be installed)
    # docker-desktop.enable = true;
  };
}
