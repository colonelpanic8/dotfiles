{ pkgs, realUsers, forEachUser, keys, ... }:
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
  ];
  extraGroupsWithWheel = extraGroups ++ ["wheel"];
  userDefaults = {
    group = "users";
    isNormalUser = true;
    createHome = true;
    shell = pkgs.zsh;
  };
in
{
  security.sudo.wheelNeedsPassword = false;
  users.users = with keys; {
    syncthing = {
      extraGroups = [ "syncthing" "wheel" ];
      home = "/var/lib/syncthing";
      createHome = true;
    };
    imalison = userDefaults // {
      extraGroups = extraGroupsWithWheel;
      name = "imalison";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = kanivanKeys;
    };
    kat = userDefaults // {
      extraGroups = extraGroupsWithWheel;
      name = "kat";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = kanivanKeys;
    };
    dean = userDefaults // {
      extraGroups = extraGroupsWithWheel;
      name = "dean";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = kanivanKeys ++ deanKeys;
    };
    will = userDefaults // {
      extraGroups = extraGroupsWithWheel;
      name = "will";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = kanivanKeys ++ willKeys;
    };
    alex = userDefaults // {
      extraGroups = extraGroupsWithWheel;
      name = "alex";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = kanivanKeys ++ alexKeys;
    };
    loewy = userDefaults // {
      inherit extraGroups;
      name = "loewy";
      openssh.authorizedKeys.keys = kanivanKeys ++ loewyKeys;
    };
    mike = userDefaults // {
      inherit extraGroups;
      name = "mike";
      openssh.authorizedKeys.keys = kanivanKeys ++ mikeKeys;
    };
    andy = userDefaults // {
      inherit extraGroups;
      name = "andy";
      openssh.authorizedKeys.keys = kanivanKeys ++ andyKeys;
    };
    micah = userDefaults // {
      inherit extraGroups;
      name = "micah";
      openssh.authorizedKeys.keys = kanivanKeys ++ micahKeys;
    };
  };

  nix.settings.trusted-users = realUsers;
  nix.sshServe = {
    enable = true;
    keys = keys.allKeys;
  };

  home-manager.users = forEachUser (import ./home-manager.nix);
}
