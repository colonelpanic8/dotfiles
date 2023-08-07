{ config, pkgs, ... }:
{
  services.avahi = {
    enable = true;
    nssmdns = true;
    publish = {
      enable = true;
      domain = true;
      workstation = true;
      userServices = true;
      addresses = true;
      hinfo = true;
    };
    extraServiceFiles = {
      ssh = "''${pkgs.avahi}/etc/avahi/services/ssh.service";
    };
  };

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
}
