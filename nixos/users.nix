{ pkgs, keys, inputs, ... }:
let
  extraGroups = [
    "adbusers"
    "audio"
    "disk"
    "docker"
    "input"
    "libvirtd"
    "libvirtd-qemu"
    "networkmanager"
    "openrazer"
    "plugdev"
    "qemu-libvirtd"
    "syncthing"
    "systemd-journal"
    "vboxusers"
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
    imalison = userDefaults // {
      extraGroups = extraGroupsWithWheel;
      name = "imalison";
      openssh.authorizedKeys.keys = kanivanKeys;
      uid = 1000;
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
    ben = userDefaults // {
      inherit extraGroups;
      name = "ben";
      openssh.authorizedKeys.keys = benKeys ++ kanivanKeys;
    };
  };

  nix.sshServe = {
    enable = true;
    keys = keys.allKeys;
  };
}
