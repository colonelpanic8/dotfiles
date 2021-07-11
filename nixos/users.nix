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
    };
    kat = userDefaults // {
      name = "kat";
      shell = pkgs.zsh;
    };
  };

  nix.trustedUsers = ["imalison" "kat"];
}
