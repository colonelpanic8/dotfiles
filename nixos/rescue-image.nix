{
  config,
  inputs,
  keys,
  lib,
  pkgs,
  rescueMode ? "iso",
  ...
}: let
  system = pkgs.stdenv.hostPlatform.system;
  isIso = rescueMode == "iso";
  rescueUser = "imalison";
  rescueHome = "/home/${rescueUser}";
  codexCli = inputs.codex-cli-nix.packages.${system}.default;
  codexDesktop = inputs.codex-desktop-linux.packages.${system}.codex-desktop-computer-use-ui-remote-mobile-control;
  claudeDesktop = pkgs.callPackage "${inputs.claude-desktop}/nix/claude-desktop.nix" {};
  rescueReadme = pkgs.writeText "nixos-rescue-readme.txt" ''
    Ivan's NixOS rescue system
    ==========================

    Graphical session
      Hyprland starts automatically. Super+Shift+Return opens Ghostty.
      Super+P opens the application launcher.

    Installer and rescue shell
      Switch to a console with Ctrl+Alt+F2. The rescue user is `imalison` and sudo
      is passwordless. Use `nmtui` for Wi-Fi and `nixos-generate-config` plus
      `nixos-install` for a manual NixOS installation.

    Persistent app state
      The writable USB installation preserves /home/imalison, /srv/dotfiles,
      and the Nix store. Nothing on the rescue system is encrypted and boot
      never prompts for an unlock passphrase. A booted ISO remains ephemeral.

    Useful commands
      cd /srv/dotfiles/nixos && just switch
      lsblk -f
      nmtui
      gparted
      cryptsetup
      nixos-generate-config --root /mnt
      nixos-install
      nixos-enter --root /mnt
  '';
in
  {
    imports = [
      inputs.home-manager.nixosModules.home-manager
      (
        if isIso
        then "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-base.nix"
        else "${inputs.nixpkgs}/nixos/modules/profiles/installation-device.nix"
      )
    ];

    nixpkgs.config.allowUnfree = true;

    # Keep the hostname aligned with the flake output so an installed writable
    # rescue system can use an ordinary `just switch` from /srv/dotfiles/nixos.
    networking.hostName =
      if isIso
      then "rescue"
      else "rescue-usb";
    networking.networkmanager.enable = true;
    time.timeZone = "America/Los_Angeles";

    nix.settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      trusted-users = [
        "root"
        rescueUser
      ];
    };

    boot.supportedFilesystems = [
      "btrfs"
      "cifs"
      "exfat"
      "ext4"
      "f2fs"
      "ntfs"
      "vfat"
      "xfs"
    ];
    hardware.enableAllHardware = true;
    hardware.enableRedistributableFirmware = true;
    services.xserver.enable = true;

    users.mutableUsers = false;
    users.users.${rescueUser} = {
      isNormalUser = true;
      createHome = true;
      home = rescueHome;
      group = "users";
      shell = pkgs.zsh;
      hashedPassword = "";
      extraGroups = [
        "audio"
        "disk"
        "input"
        "networkmanager"
        "video"
        "wheel"
      ];
      openssh.authorizedKeys.keys = keys.kanivanKeys;
    };
    security.sudo.wheelNeedsPassword = false;

    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      backupFileExtension = "hm-backup";
      extraSpecialArgs.nixos = {inherit config;};
      users.${rescueUser} = {
        imports = [./dotfiles-links.nix];
        home = {
          username = rescueUser;
          homeDirectory = rescueHome;
          stateVersion = lib.trivial.release;
        };
        programs.home-manager.enable = true;
      };
    };

    services.openssh.enable = true;
    services.gnome.gnome-keyring.enable = true;
    services.gvfs.enable = true;
    services.udisks2.enable = true;
    services.dbus.enable = true;

    security.polkit.enable = true;
    security.rtkit.enable = true;
    security.pam.services.hyprlock = {};

    services.pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };

    hardware.graphics.enable = true;
    programs.dconf.enable = true;
    programs.nix-ld.enable = true;
    programs.zsh.enable = true;

    xdg.portal = {
      enable = true;
      xdgOpenUsePortal = true;
      extraPortals = [pkgs.xdg-desktop-portal-gtk];
    };

    services.displayManager = {
      defaultSession = "hyprland-uwsm";
      autoLogin = {
        enable = true;
        user = rescueUser;
      };
    };

    # On some hybrid-GPU laptops the display manager can reach graphical.target while only
    # simpledrm exists. Hyprland then aborts before amdgpu/i915 has registered
    # the connector that drives the display. Wait for a real connected DRM
    # output, with a timeout so headless rescue use still reaches a console.
    systemd.services.rescue-gpu-ready = {
      description = "Wait for a hardware DRM display before starting Hyprland";
      before = ["display-manager.service"];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "wait-for-rescue-gpu" ''
          attempt=0
          while (( attempt < 180 )); do
            for status in /sys/class/drm/card*-*/status; do
              [ -e "$status" ] || continue
              connector="''${status%/status}"
              connector="''${connector##*/}"
              card="''${connector%%-*}"
              driver_path="$(${pkgs.coreutils}/bin/readlink -f "/sys/class/drm/$card/device/driver" 2>/dev/null || true)"
              driver="''${driver_path##*/}"
              if [ "$driver" != "simple-framebuffer" ] && ${pkgs.gnugrep}/bin/grep -qx connected "$status"; then
                echo "Hardware display $connector is ready on $driver"
                exit 0
              fi
            done
            ${pkgs.coreutils}/bin/sleep 0.25
            (( attempt += 1 ))
          done
          echo "No connected hardware DRM display appeared after 45 seconds; continuing"
        '';
      };
    };

    systemd.services.display-manager = {
      requires = ["rescue-gpu-ready.service"];
      after = ["rescue-gpu-ready.service"];
    };

    environment.sessionVariables = {
      CODEX_CLI_PATH = lib.getExe codexCli;
    };

    environment.systemPackages = with pkgs; [
      # Graphical rescue environment and the requested applications.
      emacs30-pgtk
      codexCli
      codexDesktop
      inputs.claude-code-nix.packages.${system}.default
      claudeDesktop
      # Focused disk, filesystem, hardware, and network rescue kit.
      bashInteractive
      zsh
      coreutils-full
      util-linux
      parted
      gptfdisk
      gparted
      cryptsetup
      lvm2
      mdadm
      btrfs-progs
      e2fsprogs
      xfsprogs
      dosfstools
      exfatprogs
      ntfs3g
      testdisk
      ddrescue
      smartmontools
      nvme-cli
      efibootmgr
      pciutils
      usbutils
      dmidecode
      ethtool
      wirelesstools
      networkmanager
      curl
      wget
      openssh
      rsync
      git
      just
      jq
      ripgrep
      fd
      file
      lsof
      strace
      tmux
      btop
      htop
      tree
      unzip
      zip
      p7zip
      zstd
    ];

    fonts.packages = with pkgs; [
      dejavu_fonts
      font-awesome
      nerd-fonts.iosevka
      nerd-fonts.symbols-only
    ];

    system.activationScripts.rescueUserHome = lib.stringAfter ["users"] ''
      install -d -m 0755 -o ${rescueUser} -g users \
        ${rescueHome}/.config \
        ${rescueHome}/.local \
        ${rescueHome}/.local/share \
        ${rescueHome}/.local/state \
        ${rescueHome}/Desktop

      # Seed the conventional editable location on first boot. A real checkout
      # copied onto the writable USB takes precedence and is never overwritten.
      if [ ! -e /srv/dotfiles/nixos/flake.nix ]; then
        install -d -m 0755 -o ${rescueUser} -g users /srv/dotfiles
        cp -a ${inputs.self.outPath}/. /srv/dotfiles/
        chmod -R u+rwX,g+rX /srv/dotfiles
        chown -R ${rescueUser}:users /srv/dotfiles
      fi

      ln -sfn ${rescueReadme} ${rescueHome}/Desktop/RESCUE-README.txt
      chown -h ${rescueUser}:users ${rescueHome}/Desktop/RESCUE-README.txt
    '';

    environment.etc."nixos-rescue/README".source = rescueReadme;

    system.stateVersion = lib.trivial.release;
  }
  // lib.optionalAttrs isIso {
    isoImage = {
      edition = lib.mkForce "rescue";
      volumeID = "NIX_RESCUE";
    };
    image.fileName = lib.mkForce "imalison-nixos-rescue-${config.system.nixos.release}-${system}.iso";
  }
