{
  keys,
  inputs,
  lib,
  config,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    inputs.agenix.nixosModules.default
    ../profiles/standalone.nix
    ../profiles/standalone-desktop.nix
    ../tailscale.nix
    ../upterm-backup.nix
  ];

  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
  boot.kernelModules = ["kvm-intel"];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/3d85f79b-49e5-4065-b270-a87f0e9cfbfc";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/9E7F-22B1";
    fsType = "vfat";
    options = ["fmask=0077" "dmask=0077"];
  };
  swapDevices = [{device = "/dev/disk/by-uuid/b078b443-a59a-4404-8016-302a5295cc15";}];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # AD106 / RTX 4060 Ti supports NVIDIA's open kernel module.
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia = {
    open = true;
    modesetting.enable = true;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  # Carried over from the installer-generated configuration.
  networking.networkmanager.enable = true;
  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = lib.genAttrs [
    "LC_ADDRESS"
    "LC_IDENTIFICATION"
    "LC_MEASUREMENT"
    "LC_MONETARY"
    "LC_NAME"
    "LC_NUMERIC"
    "LC_PAPER"
    "LC_TELEPHONE"
    "LC_TIME"
  ] (_: "en_US.UTF-8");
  services.desktopManager.gnome.enable = true;
  services.printing.enable = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  programs.firefox.enable = true;
  nixpkgs.config.allowUnfree = true;
  system.stateVersion = "26.05";

  myModules.primaryUser = "alex";
  users.users.alex = {
    isNormalUser = true;
    description = "Alexander Malison";
    extraGroups = ["networkmanager" "wheel"];
  };
  myModules.uptermBackup.enable = true;
  myModules.hostIdentity = {
    emoticon = "🎤";
    tmux.background = "#6d28d9";
  };
  networking.hostName = lib.mkForce "msi-elliott";
  users.users.alex.openssh.authorizedKeys.keys = keys.userKeys.alex;
  home-manager.users.alex.programs.git.settings.user = {
    name = "Alexander Malison";
    email = "alexmalison@gmail.com";
  };

  # Keep this machine reachable regardless of desktop power-management settings.
  systemd.sleep.settings.Sleep = {
    AllowSuspend = false;
    AllowHibernation = false;
    AllowHybridSleep = false;
    AllowSuspendThenHibernate = false;
  };
  systemd.targets = lib.genAttrs [
    "sleep"
    "suspend"
    "hibernate"
    "hybrid-sleep"
    "suspend-then-hibernate"
  ] (_: {enable = false;});
  services.logind.settings.Login = {
    IdleAction = "ignore";
    HandleSuspendKey = "ignore";
    HandleHibernateKey = "ignore";
    HandleLidSwitch = "ignore";
    HandleLidSwitchExternalPower = "ignore";
    HandleLidSwitchDocked = "ignore";
  };
  # Locked so Plasma's power settings cannot reintroduce sleep actions.
  environment.etc."xdg/powerdevilrc".text = lib.concatMapStrings (profile: ''
    [${profile}][SuspendAndShutdown][$i]
    AutoSuspendAction=0
    LidAction=0
    PowerButtonAction=16

  '') ["AC" "Battery" "LowBattery"];

  home-manager.users.alex.dconf.settings."org/gnome/settings-daemon/plugins/power" = {
    sleep-inactive-ac-type = "nothing";
    sleep-inactive-battery-type = "nothing";
    sleep-inactive-ac-timeout = 0;
    sleep-inactive-battery-timeout = 0;
  };

  # Retain the existing tailnet identity, state directory, and SSH access.
  services.tailscale.enable = lib.mkForce true;
  services.openssh.enable = lib.mkForce true;
  # Only this machine's host key can decrypt its Alex enrollment credential.
  myModules.tailscale.enrollment = "alex";
  myModules.tailscale.enrollmentProfiles.alex = {
    authKeyFile = null;
    authKeySecret = ../secrets/tailscale-authkey.msi-elliott.age;
  };
  age.identityPaths = lib.mkForce ["/etc/ssh/ssh_host_ed25519_key"];
  age.secrets.github-cli-token = {
    file = ../secrets/github-cli-token.msi-elliott.age;
    owner = "alex";
    mode = "0400";
  };
  # SSH sessions must not depend on an unlocked graphical-session keyring.
  environment.systemPackages = [
    (lib.hiPrio (pkgs.writeShellScriptBin "gh" ''
      if [ -z "''${GH_TOKEN:-}" ] && [ -z "''${GITHUB_TOKEN:-}" ] && [ -r ${config.age.secrets.github-cli-token.path} ]; then
        export GH_TOKEN="$(< ${config.age.secrets.github-cli-token.path})"
      fi
      exec ${lib.getExe pkgs.gh} "$@"
    ''))
  ];
  systemd.services.tailscaled = {
    stopIfChanged = false;
    restartIfChanged = false;
  };
  systemd.services.NetworkManager = {
    stopIfChanged = false;
    restartIfChanged = false;
  };
}
