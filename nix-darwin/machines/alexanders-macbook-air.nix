{pkgs, ...}: {
  imports = [
    (import ../tailscale.nix {
      authKeyFile = ../../nixos/secrets/tailscale-authkey.alexanders-macbook-air.age;
    })
  ];

  networking.hostName = "alexanders-macbook-air";
  system.stateVersion = 7;
  age.identityPaths = ["/etc/ssh/ssh_host_ed25519_key"];
  environment.systemPackages = [pkgs.bitwarden-desktop];

  users.users.alex = {
    name = "alex";
    home = "/Users/alex";
    openssh.authorizedKeys.keys = (import ../../nix-shared/keys.nix).userKeys.alex;
  };
}
