{ pkgs, realUsers, forEachUser, kanivanKeys, deanKeys, alexKeys, allKeys, ... }:
let
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
    userDefaults = {
      inherit extraGroups;
      group = "users";
      isNormalUser = true;
      createHome = true;
      shell = pkgs.zsh;
    };
in
{
  security.sudo.wheelNeedsPassword = false;
  users.users = {
    syncthing = {
      extraGroups = [ "syncthing" "wheel" ];
      home = "/var/lib/syncthing";
      createHome = true;
    };
    imalison = userDefaults // {
      name = "imalison";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = kanivanKeys;
    };
    kat = userDefaults // {
      name = "kat";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = kanivanKeys;
    };
    dean = userDefaults // {
      name = "dean";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = kanivanKeys ++ deanKeys;
    };
    alex = userDefaults // {
      name = "alex";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = kanivanKeys ++ alexKeys;
    };
  };

  nix.settings.trusted-users = realUsers;
  nix.sshServe = {
    enable = true;
    keys = allKeys;
  };

  home-manager.users = forEachUser (import ./home-manager.nix);
}
