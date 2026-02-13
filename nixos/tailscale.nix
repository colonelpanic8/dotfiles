{ config, lib, pkgs, makeEnable, ... }:
makeEnable config "myModules.tailscale" true {
  # Provide stable SSH connectivity between your machines without needing port
  # forwarding (works behind NAT/CGNAT).
  services.tailscale.enable = true;

  # Handy even if you only enable the service and run `tailscale up` manually.
  environment.systemPackages = [ pkgs.tailscale ];
}

