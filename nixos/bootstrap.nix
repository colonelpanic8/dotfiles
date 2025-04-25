{ pkgs, ... }:
{
  imports = [
    ./essential.nix
  ];
  environment.systemPackages = with pkgs; [
    emacs
  ];
  programs.zsh.enable = true;
  networking.firewall.enable = false;
  networking.networkmanager = {
    enable = true;
    extraConfig = ''
      [main]
      rc-manager=resolvconf
    '';
  };
  nixpkgs.config.allowUnfree = true;
  services.xserver = {
    exportConfiguration = true;
    enable = true;
    layout = "us";
    desktopManager = {
      plasma6.enable = true;
    };
    displayManager = {
      sddm = {
        enable = true;
      };
      sessionCommands = ''
        systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
      '';
      setupCommands = ''
        autorandr -c
        systemctl restart autorandr.service
      '';
    };
  };
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  users.users = {
    imalison = {
      extraGroups = [
        "audio"
        "adbusers"
        "disk"
        "docker"
        "networkmanager"
        "openrazer"
        "plugdev"
        "syncthing"
        "systemd-journal"
        "video"
        "wheel"
      ];
      group = "users";
      isNormalUser = true;
      createHome = true;
      shell = pkgs.zsh;
    };
  };
}
