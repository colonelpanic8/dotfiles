{
  lib,
  pkgs,
  ...
}: {
  # A catalog, not an enrollment grant: only the profile selected by a machine
  # is provisioned. Each field can be overridden in that machine's module.
  myModules.tailscale.enrollmentProfiles = {
    ivan = {
      authKeySecret = lib.mkDefault ../../nixos/secrets/tailscale-authkey.age;
      operator = lib.mkDefault "imalison";
      reauthenticateExpired = lib.mkDefault true;
      identityPaths = lib.mkDefault (lib.optionals pkgs.stdenv.hostPlatform.isLinux [
        "/etc/ssh/ssh_host_ed25519_key"
        "/home/imalison/.ssh/id_ed25519"
      ]);
    };
    alex = {
      authKeyFile = lib.mkDefault "/var/lib/tailscale-alex/authkey";
      operator = lib.mkDefault "alex";
    };
    kat = {
      authKeySecret = lib.mkDefault ../../nixos/secrets/tailscale-authkey.kats-macbook-neo.age;
      operator = lib.mkDefault "kat";
    };
  };
}
