{ pkgs, keys, inputs, system, ... }:
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
      openssh.authorizedKeys.keys = [giteaSecret] ++ kanivanKeys;
    };
    ivanm = userDefaults // {
      extraGroups = extraGroupsWithWheel;
      name = "ivanm";
      openssh.authorizedKeys.keys = kanivanKeys;
    };
    imalison = userDefaults // {
      extraGroups = extraGroupsWithWheel;
      name = "imalison";
      openssh.authorizedKeys.keys = kanivanKeys;
    };
    kat = userDefaults // {
      extraGroups = extraGroupsWithWheel;
      name = "kat";
      openssh.authorizedKeys.keys = kanivanKeys;
    };
    dean = userDefaults // {
      extraGroups = extraGroupsWithWheel;
      name = "dean";
      openssh.authorizedKeys.keys = kanivanKeys ++ deanKeys;
    };
    will = userDefaults // {
      extraGroups = extraGroupsWithWheel;
      name = "will";
      openssh.authorizedKeys.keys = kanivanKeys ++ willKeys;
    };
    alex = userDefaults // {
      extraGroups = extraGroupsWithWheel;
      name = "alex";
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
    unprivileged = userDefaults // {
      extraGroups = ["syncthing"];
      name = "unprivileged";
      openssh.authorizedKeys.keys = [giteaSecret] ++ kanivanKeys;
    };
    ben = userDefaults // {
      inherit extraGroups;
      name = "ben";
      openssh.authorizedKeys.keys = benKeys ++ kanivanKeys;
    };
    railbird = userDefaults // {
      inherit extraGroups;
      name = "railbird";
      openssh.authorizedKeys.keys = inputs.railbird-secrets.keys.railbirdAdminKeys;
    };
  };

  nix.sshServe = {
    enable = true;
    keys = keys.allKeys;
  };
}
