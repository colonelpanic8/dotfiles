{ pkgs, ... }:
{
  security.sudo.wheelNeedsPassword = false;
  users.extraUsers = let
    extraGroups = [
      "audio"
      "adbusers"
      "disk"
      "docker"
      "networkmanager"
      "plugdev"
      "systemd-journal"
      "video"
      "wheel"
    ];
    userDefaults = {
      inherit extraGroups;
      group = "users";
      isNormalUser = true;
      createHome = true;
      shell = pkgs.zsh;
    };
  in {
    imalison = userDefaults // {
      name = "imalison";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICzGkqGJm+nrMvsrfuWOLVxXHvi0UL1ULJmyfzS9sKpy imalison@biskcomp.local"
      ];
    };
    kat = userDefaults // {
      name = "kat";
      shell = pkgs.zsh;
    };
  };

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.imalison = import ./home-manager.nix;

  nix.trustedUsers = ["imalison" "kat"];
}
