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
        openssh.authorizedKeys.keys = sshClientKeys;
      };
    kat =
      userDefaults
      // {
        extraGroups = extraGroupsWithWheel;
        name = "kat";
        openssh.authorizedKeys.keys = sshClientKeys;
      };
    dean =
      userDefaults
      // {
        extraGroups = extraGroupsWithWheel;
        name = "dean";
        openssh.authorizedKeys.keys = sshClientKeys ++ deanKeys;
      };
    alex =
      userDefaults
      // {
        extraGroups = extraGroupsWithWheel;
        name = "alex";
        openssh.authorizedKeys.keys = sshClientKeys ++ alexKeys;
      };
    loewy =
      userDefaults
      // {
        inherit extraGroups;
        name = "loewy";
        openssh.authorizedKeys.keys = sshClientKeys ++ loewyKeys;
      };
    ben =
      userDefaults
      // {
        inherit extraGroups;
        name = "ben";
        openssh.authorizedKeys.keys = benKeys ++ sshClientKeys;
      };
  };

  nix.sshServe = {
    enable = true;
    keys = keys.allKeys;
  };
}
