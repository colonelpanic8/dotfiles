{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix")
    ./calamares.nix
  ];

  environment.systemPackages = [pkgs.just pkgs.tailscale];
  services.tailscale.enable = true;
  isoImage.edition = lib.mkForce "family";
  image.fileName = lib.mkForce "nixos-family-installer-${config.system.nixos.release}-x86_64-linux.iso";
}
