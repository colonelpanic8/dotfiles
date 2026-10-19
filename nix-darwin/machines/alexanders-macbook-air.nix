{pkgs, ...}: {
  networking.hostName = "alexanders-macbook-air";
  system.stateVersion = 7;
  environment.systemPackages = [pkgs.bitwarden-desktop];

  users.users.alex = {
    name = "alex";
    home = "/Users/alex";
    openssh.authorizedKeys.keys = (import ../../nix-shared/keys.nix).userKeys.alex;
  };
}
