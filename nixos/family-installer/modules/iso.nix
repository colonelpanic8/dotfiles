{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: let
  unlock = pkgs.writeShellApplication {
    name = "family-unlock";
    runtimeInputs = [pkgs.python3 pkgs.age pkgs.openssh pkgs.tailscale];
    text = ''
      exec python3 ${../scripts}/provisioning.py unlock /iso/family-seed.age nixos
    '';
  };
  start = pkgs.writeShellApplication {
    name = "family-start";
    runtimeInputs = [pkgs.coreutils];
    text = ''
      if [[ -f /iso/family-seed.age && ! -f /run/family-installer.ready ]]; then
        sudo ${lib.getExe unlock}
      fi
      mkdir -p "$HOME/Setup"
      install -m 0644 /etc/nixos-family-installer/{README.md,AGENTS.md,START-HERE.md} "$HOME/Setup/"
      cd "$HOME/Setup"
      exec /run/current-system/sw/bin/t3code-desktop --password-store=basic
    '';
  };
  desktop = pkgs.makeDesktopItem {
    name = "family-start";
    desktopName = "Start personal setup";
    exec = "${pkgs.kdePackages.konsole}/bin/konsole --hold -e ${lib.getExe start}";
    terminal = false;
    icon = "utilities-terminal";
    categories = ["System"];
  };
in {
  imports = [
    (modulesPath + "/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix")
    ./calamares.nix
    ./agent-tools.nix
  ];

  environment.systemPackages = [pkgs.just pkgs.tailscale unlock start desktop];
  environment.etc."xdg/autostart/family-setup.desktop".source = "${desktop}/share/applications/family-start.desktop";
  environment.etc."xdg/autostart/calamares.desktop".text = ''
    [Desktop Entry]
    Type=Application
    Name=Install System
    Hidden=true
  '';
  environment.etc."nixos-family-installer/START-HERE.md".source = ../START-HERE.md;
  systemd.tmpfiles.settings."10-installer-desktop" = {
    "/home/nixos/Desktop/family-start.desktop"."L+".argument = "${desktop}/share/applications/family-start.desktop";
    "/home/nixos/Desktop/calamares.desktop"."L+".argument = lib.mkForce "/run/current-system/sw/share/applications/calamares.desktop";
  };
  familyInstaller.agentTools.enable = true;
  services.tailscale.enable = true;
  services.openssh.openFirewall = lib.mkForce false;
  networking.firewall.interfaces.tailscale0.allowedTCPPorts = [22];
  isoImage.edition = lib.mkForce "family";
  image.fileName = lib.mkForce "nixos-family-installer-${config.system.nixos.release}-x86_64-linux.iso";
}
