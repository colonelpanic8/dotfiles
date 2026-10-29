{
  config,
  inputs,
  keys,
  lib,
  pkgs,
  ...
}: let
  system = pkgs.stdenv.hostPlatform.system;
  user = "imalison";
  home = "/home/${user}";
  codexCli = inputs.codex-cli-nix.packages.${system}.default;
  rescueReadme = pkgs.writeText "nixos-rescue-readme.txt" ''
    Ivan's portable NixOS USB
    =========================

    This is the shared nixos-portable stick plus Ivan's personal layer from
    dotfiles (account, Hyprland, Tailscale enrollment, and desktop agent apps).

    Graphical session
      Hyprland starts automatically. Super+Shift+Return opens Ghostty.
      Super+P opens the application launcher.

    Family installer
      Start personal setup unlocks a bundle from the SETUP volume (/setup) and
      opens T3 Code in a separate, in-memory home, so this stick's own logins
      are never handed to the installed computer. Install System opens
      Calamares. Read /etc/nixos-family-installer/README.md first. Agents must
      also read /etc/nixos-family-installer/AGENTS.md. Confirm the destination
      disk before Install; Calamares refuses this stick itself.

    Rescue shell and remote help
      Switch to a console with Ctrl+Alt+F2. Sudo is passwordless. Use `nmtui`
      for Wi-Fi. Tailscale state persists on this stick.

    Persistent state
      /home/imalison, /srv/dotfiles, and the Nix store persist. Nothing on the
      stick is encrypted and boot never prompts for an unlock passphrase.

    Useful commands
      cd /srv/dotfiles/nixos && just switch
      lsblk -f
      nmtui
      gparted
  '';
in {
  imports = [
    inputs.nixos-portable.nixosModules.portable
    inputs.home-manager.nixosModules.home-manager
    inputs.agenix.nixosModules.default
    ../options.nix
    ../environment.nix
    ../display-manager.nix
    ../desktop.nix
    ../taffybar.nix
    ../sni.nix
    ../chrome-favicon-dbus.nix
    ../waybar.nix
    ../tailscale.nix
  ];

  # A deliberately small, portable module set on top of the shared stick. Do
  # not import configuration.nix here: that would enable many
  # workstation/server assumptions that do not belong on a USB stick.
  networking.hostName = "rescue-usb";
  dotfilesEnvironment.enableHostPythonCompletions = false;
  myModules.displayManager.enable = true;
  myModules.desktop = {
    enable = true;
    profile = "minimal";
  };
  myModules.fonts.enable = false;
  myModules.hyprland.portable = true;
  myModules.movaCapture.enable = false;
  myModules.taffybar.profile = "minimal";
  myModules.hostIdentity = {
    emoticon = "🛟";
    tmux.background = "#0369a1";
  };

  portable = {
    liveUser = user;
    desktop = "none";
    terminal = ["${pkgs.ghostty}/bin/ghostty" "-e"];
    tailscale.ephemeral = false;
  };
  familyInstaller.agentTools.packages = [
    inputs.t3code-integration.packages.${system}.t3code
    codexCli
    inputs.claude-code-nix.packages.${system}.default
  ];
  image.repart.name = "imalison-nixos-rescue-usb";

  nixpkgs.config.allowUnfree = true;
  # This machine skips configuration.nix, and with it the overlay in nix.nix.
  # Enabling myModules.desktop still makes dotfiles-links.nix reach for
  # pkgs.computer-use-linux, so provide just that package.
  nixpkgs.overlays = [
    (final: _prev: {
      computer-use-linux = final.callPackage ../packages/computer-use-linux {
        src = inputs.computer-use-linux;
      };
    })
  ];

  boot.loader.systemd-boot = {
    # MemTest86 reports which DIMM slot holds a failing address; memtest86+
    # (from the shared stick) cannot.
    extraFiles."efi/memtest86/memtest86.efi" = "${pkgs.memtest86-efi}/BOOTX64.efi";
    extraEntries."memtest86-passmark.conf" = ''
      title MemTest86 (PassMark, free)
      efi /efi/memtest86/memtest86.efi
    '';
  };

  users.mutableUsers = false;
  users.users.${user} = {
    createHome = true;
    inherit home;
    group = "users";
    shell = pkgs.zsh;
    initialHashedPassword = lib.mkForce null;
    hashedPassword = "";
    openssh.authorizedKeys.keys = keys.sshClientKeys;
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "hm-backup";
    extraSpecialArgs.nixos = {inherit config;};
    users.${user} = {
      imports = [../dotfiles-links.nix];
      home = {
        username = user;
        homeDirectory = home;
        stateVersion = lib.trivial.release;
      };
      programs.home-manager.enable = true;
    };
  };

  services.gnome.gnome-keyring.enable = true;
  services.displayManager.defaultSession = "hyprland-uwsm";
  services.xserver.enable = true;
  security.pam.services.hyprlock = {};
  programs.dconf.enable = true;
  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
  };

  environment.sessionVariables.CODEX_CLI_PATH = lib.getExe codexCli;
  environment.systemPackages = [
    pkgs.emacs-pgtk
    inputs.codex-desktop-linux.packages.${system}.codex-desktop-computer-use-ui-remote-mobile-control
    (pkgs.callPackage "${inputs.claude-desktop}/nix/claude-desktop.nix" {})
    inputs.paseo.packages.${system}.default
    inputs.paseo.packages.${system}.desktop
  ];

  fonts.packages = with pkgs; [
    dejavu_fonts
    font-awesome
    nerd-fonts.iosevka
    nerd-fonts.symbols-only
  ];

  system.activationScripts.rescueUserHome = lib.stringAfter ["users"] ''
    install -d -m 0755 -o ${user} -g users \
      ${home}/.config \
      ${home}/.local \
      ${home}/.local/share \
      ${home}/.local/state \
      ${home}/Desktop

    # Seed the conventional editable location on first boot. A real checkout
    # copied onto the stick takes precedence and is never overwritten.
    if [ ! -e /srv/dotfiles/nixos/flake.nix ]; then
      install -d -m 0755 -o ${user} -g users /srv/dotfiles
      cp -a ${../..}/. /srv/dotfiles/
      chmod -R u+rwX,g+rX /srv/dotfiles
      chown -R ${user}:users /srv/dotfiles
    fi

    ln -sfn ${rescueReadme} ${home}/Desktop/RESCUE-README.txt
    ln -sfn /etc/nixos-family-installer/README.md ${home}/Desktop/FAMILY-INSTALLER-README.md
    chown -h ${user}:users \
      ${home}/Desktop/RESCUE-README.txt \
      ${home}/Desktop/FAMILY-INSTALLER-README.md
  '';

  environment.etc."nixos-rescue/README".source = rescueReadme;
}
