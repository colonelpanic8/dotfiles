{ inputs, ... }:
{
  imports = [
    inputs.nixos-wsl.nixosModules.wsl
    ./nix.nix
    ./users.nix
    ./essential.nix
    ./environment.nix
  ];

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  wsl = {
    enable = true;
    automountPath = "/mnt";
    startMenuLaunchers = true;

    # Enable native Docker support
    # docker-native.enable = true;

    # Enable integration with Docker Desktop (needs to be installed)
    # docker-desktop.enable = true;
  };
}
