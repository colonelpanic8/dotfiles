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
  extraGroupsWithWheel = extraGroups ++ ["wheel"];
  userDefaults = {
    group = "users";
    isNormalUser = true;
    createHome = true;
    shell = pkgs.zsh;
  };
in {
  security.sudo.wheelNeedsPassword = false;
  users.users = with keys; {
    imalison =
      userDefaults
      // {
        extraGroups = extraGroupsWithWheel ++ ["dialout"];
        name = "imalison";
        openssh.authorizedKeys.keys = userKeys.imalison;
      };
    kat =
      userDefaults
      // {
        extraGroups = extraGroupsWithWheel;
        name = "kat";
        openssh.authorizedKeys.keys = userKeys.kat;
      };
    dean =
      userDefaults
      // {
        extraGroups = extraGroupsWithWheel;
        name = "dean";
        openssh.authorizedKeys.keys = userKeys.dean;
      };
    alex =
      userDefaults
      // {
        extraGroups = extraGroupsWithWheel;
        name = "alex";
        openssh.authorizedKeys.keys = userKeys.alex;
      };
    loewy =
      userDefaults
      // {
        inherit extraGroups;
        name = "loewy";
        openssh.authorizedKeys.keys = userKeys.loewy;
      };
    ben =
      userDefaults
      // {
        inherit extraGroups;
        name = "ben";
        openssh.authorizedKeys.keys = userKeys.ben;
      };
  };

  nix.sshServe = {
    enable = true;
    keys = keys.allKeys;
  };
}
