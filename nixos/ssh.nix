{pkgs, ...}: {
  # TODO: reenable
  # security.pam.sshAgentAuth.enable = true;
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    nssmdns6 = true;
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
    settings = {
      AllowAgentForwarding = true;
      AllowTcpForwarding = true;
      PasswordAuthentication = false;
      X11Forwarding = true;
    };
  };

  programs.ssh = {
    # OpenSSH rejects the systemd proxy snippet when the Nix store is mounted
    # with remapped ownership (as on the shared dotfiles hosts), preventing all
    # ordinary SSH commands before host matching even begins. We do not use
    # systemd-ssh-proxy; direct DNS/Tailscale hostnames cover the fleet.
    systemd-ssh-proxy.enable = false;
    forwardX11 = false;
    setXAuthLocation = true;
    knownHosts = {
      github = {
        hostNames = ["github.com"];
        publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl";
      };
      gitlab = {
        hostNames = ["gitlab.com"];
        publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAfuCHKVTjquxvt6CM6tdG4SLp1Btn/nOeHHE5UOzRdf";
      };
    };
  };
}
