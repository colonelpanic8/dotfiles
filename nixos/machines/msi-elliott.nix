{
  keys,
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    inputs.agenix.nixosModules.default
    ../hardware/msi-elliott/installed-configuration.nix
    ../hardware/msi-elliott/nvidia.nix
    ../profiles/standalone.nix
    ../profiles/standalone-desktop.nix
    ../tailscale.nix
    ../upterm-backup.nix
  ];

  myModules.primaryUser = "alex";
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
