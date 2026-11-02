pkgs: rec {
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
    "uinput"
    "vboxusers"
    "video"
  ];
  # Each of these grants root or reads imalison's data (raw disks, container
  # and VM daemons, the synced home folders, all logs, keystrokes, synthetic
  # input).
  privilegedGroups = [
    "disk"
    "docker"
    "input"
    "libvirtd"
    "libvirtd-qemu"
    "qemu-libvirtd"
    "syncthing"
    "systemd-journal"
    "uinput"
  ];
  guestGroups = builtins.filter (g: !(builtins.elem g privilegedGroups)) extraGroups;
  guestGroupsWithWheel = guestGroups ++ ["wheel"];
  userDefaults = {
    group = "users";
    isNormalUser = true;
    createHome = true;
    shell = pkgs.zsh;
  };
}
