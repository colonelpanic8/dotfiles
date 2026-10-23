{
  pkgs,
  keys,
  inputs,
  ...
}: let
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
  # Each of these grants root or reads imalison's data (raw disks, container
  # and VM daemons, the synced home folders, all logs, keystrokes).
  privilegedGroups = [
    "disk"
    "docker"
    "input"
    "libvirtd"
    "libvirtd-qemu"
    "qemu-libvirtd"
    "syncthing"
    "systemd-journal"
  ];
  guestGroups = builtins.filter (g: !(builtins.elem g privilegedGroups)) extraGroups;
  guestGroupsWithWheel = guestGroups ++ ["wheel"];
  userDefaults = {
    group = "users";
    isNormalUser = true;
    createHome = true;
    shell = pkgs.zsh;
  };
in {
  security.sudo.wheelNeedsPassword = true;
  security.sudo.extraRules = [
    {
      users = ["imalison"];
      commands = [
        {
          command = "ALL";
          options = ["NOPASSWD" "SETENV"];
        }
      ];
    }
  ];
  users.users = with keys; {
    imalison =
      userDefaults
      // {
        extraGroups = extraGroups ++ ["wheel" "dialout"];
        name = "imalison";
        openssh.authorizedKeys.keys = userKeys.imalison;
      };
    kat =
      userDefaults
      // {
        extraGroups = guestGroupsWithWheel;
        name = "kat";
        openssh.authorizedKeys.keys = userKeys.kat;
      };
    dean =
      userDefaults
      // {
        extraGroups = guestGroupsWithWheel;
        name = "dean";
        openssh.authorizedKeys.keys = userKeys.dean;
      };
    alex =
      userDefaults
      // {
        extraGroups = guestGroupsWithWheel;
        name = "alex";
        openssh.authorizedKeys.keys = userKeys.alex;
      };
    loewy =
      userDefaults
      // {
        extraGroups = guestGroups;
        name = "loewy";
        openssh.authorizedKeys.keys = userKeys.loewy;
      };
    ben =
      userDefaults
      // {
        extraGroups = guestGroups;
        name = "ben";
        openssh.authorizedKeys.keys = userKeys.ben;
      };
  };

  nix.sshServe = {
    enable = true;
    keys = keys.allKeys;
  };
}
