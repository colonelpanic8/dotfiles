{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  user = config.myModules.primaryUser;
  worktree = config.dotfiles-worktree;
  codexCli = inputs.codex-cli-nix.packages.${pkgs.stdenv.hostPlatform.system}.default;
  claudeDesktop = (pkgs.callPackage "${inputs.claude-desktop}/nix/claude-desktop.nix" {}).overrideAttrs (previous: {
    postFixup =
      (previous.postFixup or "")
      + ''
        wrapProgram "$out/bin/claude-desktop" \
          --add-flags "--password-store=gnome-libsecret"
      '';
  });
  # The t3code flake wraps the codex from its own nixpkgs into PATH, which lags
  # codex-cli-nix and hides newer models. Build the same package against that
  # nixpkgs with the system's codex swapped in.
  t3Base =
    (import "${inputs.t3code}/nix/package.nix" {
      buildCommit = inputs.t3code.rev or "";
      pkgs = import inputs.t3code.inputs.nixpkgs {
        system = pkgs.stdenv.hostPlatform.system;
        config.allowUnfree = true;
        overlays = [(_: _: {codex = codexCli;})];
      };
      self = inputs.t3code;
    }).t3code;
  t3Unwrapped = t3Base.unwrapped.overrideAttrs (previous: {
    # Avoid overwhelming the Wi-Fi/router DNS resolver during the large fetch.
    # The upstream fixed-output hash remains unchanged.
    pnpmDeps = previous.pnpmDeps.overrideAttrs (deps: {
      env =
        (deps.env or {})
        // {
          pnpm_config_network_concurrency = "8";
          pnpm_config_fetch_retries = "5";
          pnpm_config_fetch_timeout = "120000";
        };
    });
  });
  t3Package = t3Base.overrideAttrs (_: {paths = [t3Unwrapped];});
in {
  imports = [
    # Hyprland's pinned inputs use a newer libc than this machine's base OS.
    # Build its compositor and plugins together with that input's toolchain,
    # while leaving the system and connectivity packages on their existing pin.
    ({
      config,
      lib,
      makeEnable,
      inputs,
      ...
    }:
      import ../hyprland.nix {
        inherit config lib makeEnable inputs;
        pkgs = inputs.hyprland.inputs.nixpkgs.legacyPackages.x86_64-linux;
      })
    ../claude-mcp.nix
    ../computer-use-linux.nix
    ../dux/shell.nix
    ../fonts.nix
    ../electron.nix
    ../remote-hyprland.nix
  ];

  # Layered on the standalone profile; no workstation accounts or secrets are imported.
  options.myModules.taffybar.enable = lib.mkEnableOption "Taffybar";

  config = {
    nixpkgs.overlays = [
      (final: prev: {
        playwright-cli = final.callPackage ../packages/playwright-cli {};
        # Removed from nixpkgs; take only the workstation overlay's fork build.
        inherit ((import ../../nix-shared/overlays) final prev) rofi-pass;
      })
      # On this nixpkgs pin, fastmcp's stateful proxy tests fail deterministically
      # (anyio cancel-scope errors), which blocks building mcp-nixos.
      (_: prev: {
        pythonPackagesExtensions =
          prev.pythonPackagesExtensions
          ++ [
            (_: python: {
              fastmcp = python.fastmcp.overridePythonAttrs (previous: {
                disabledTestPaths = (previous.disabledTestPaths or []) ++ ["tests/server/providers/proxy/test_stateful_proxy_client.py"];
              });
            })
          ];
      })
    ];
    myModules.taffybar.enable = lib.mkForce false;
    myModules.hyprland = {
      enable = true;
      # Use automatic monitor discovery rather than another host's dock layout.
      portable = true;
    };
    myModules.fonts.enable = true;
    myModules.computerUseLinux.enable = true;
    myModules.electron.enable = true;

    # Desktop agents download native binaries that need the standard ELF loader.
    programs.nix-ld.enable = true;
    hardware.uinput.enable = true;
    users.users.${user}.extraGroups = ["input" "uinput"];
    programs.ydotool = {
      enable = true;
      group = "input";
    };
    services.udev.packages = [
      (pkgs.writeTextDir "lib/udev/rules.d/70-uinput-uaccess.rules" ''
        KERNEL=="uinput", SUBSYSTEM=="misc", TAG+="uaccess", OPTIONS+="static_node=uinput"
      '')
    ];

    services.displayManager.gdm.enable = lib.mkForce false;
    services.displayManager.sddm.enable = true;
    services.displayManager.defaultSession = "hyprland-uwsm";
    services.desktopManager.plasma6.enable = true;
    services.gnome.gnome-keyring.enable = true;
    programs.dconf.enable = true;
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;
    services.gvfs.enable = true;
    services.tumbler.enable = true;
    services.udisks2.enable = true;
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
    };

    # A second, VNC-only Hyprland session with the user's own config, running
    # from boot independently of the greeter and the physical session.
    myModules.remote-hyprland = {
      enable = true;
      nested = true;
      autoStart = true;
      inherit user;
      bindAddress = "0.0.0.0";
      configFile = "/home/${user}/.config/hypr/hyprland.lua";
      # The regular dms.service belongs to the physical graphical session.
      shellCommand = "${config.system.build.dms}/bin/dms run";
      shellEnvironment = config.home-manager.users.${user}.systemd.user.services.dms.Service.Environment;
    };
    networking.firewall.interfaces.tailscale0.allowedTCPPorts = [config.myModules.remote-hyprland.port 3773];

    # The shared portable developer toolset plus the regular desktop, office,
    # communications, creative, browser, and media applications.
    environment.systemPackages =
      (import ../../nix-shared/portable-packages.nix {inherit pkgs;})
      ++ [
        codexCli
        claudeDesktop
        inputs.claude-code-nix.packages.${pkgs.stdenv.hostPlatform.system}.default
        t3Package
      ]
      ++ (with pkgs; [
        adwaita-icon-theme
        alacritty
        android-studio
        asciidoctor
        autorandr
        blueman
        bubblewrap
        dex
        dpkg
        efibootmgr
        gdb
        gparted
        inotify-tools
        iotop
        lshw
        mesa-demos
        patchelf
        pciutils
        sshfs
        sysz
        udiskie
        usbutils
        wl-clipboard
        brightnessctl
        cheese
        chromium
        emacs-pgtk
        espeak
        feh
        file-roller
        firefox
        ghostty
        gimp
        git-lfs
        google-chrome
        gource
        gradle
        graphviz
        gthumb
        hicolor-icon-theme
        inkscape
        kdePackages.dolphin
        kdePackages.kleopatra
        kdePackages.okular
        kdePackages.qt6ct
        kdePackages.xdg-desktop-portal-kde
        libreoffice
        libnotify
        loupe
        lxappearance
        mermaid-cli
        networkmanagerapplet
        obs-studio
        pandoc
        papirus-icon-theme
        pavucontrol
        picard
        pinentry-gnome3
        playerctl
        pulsemixer
        remmina
        rofi
        rofi-pass
        rofi-systemd
        signal-desktop
        slack
        spotify
        texliveSmall
        thunar
        tor-browser
        transmission_4-gtk
        tumbler
        vlc
        vscode
        wmctrl
        wofi
        xclip
        xdotool
        xsettingsd
        yt-dlp
      ]);

    home-manager.users.${user} = {config, ...}: let
      link = config.lib.file.mkOutOfStoreSymlink;
    in {
      imports = [
        inputs.codex-desktop-linux.homeManagerModules.default
        ../../nix-shared/home-manager/codex-generated-skills.nix
      ];
      # Only the managed MCP servers; the shared Codex config is personal.
      myModules.codexGeneratedSkills = {
        enable = true;
        sharedDotfiles = false;
        computerUseLinuxPackage = pkgs.computer-use-linux;
      };
      programs.codexDesktopLinux = {
        enable = true;
        cliPackage = codexCli;
        computerUseUi.enable = true;
        linuxFeatures = ["shallow-repository-watches"];
      };
      home.file = lib.genAttrs [".claude/skills/computer-use-linux" ".codex/skills/computer-use-linux"] (_: {
        source = link "${worktree}/dotfiles/agents/skills/computer-use-linux";
      });
      systemd.user.sessionVariables.YDOTOOL_SOCKET = "/run/ydotoold/socket";
      xdg.configFile = {
        "hypr".source = link "${worktree}/dotfiles/config/hypr";
        "rofi".source = link "${worktree}/dotfiles/config/rofi";
        "alacritty".source = link "${worktree}/dotfiles/config/alacritty";
        "ghostty/config".text = ''
          confirm-close-surface = false
          shell-integration-features = cursor,no-sudo,no-title,no-ssh-env,no-ssh-terminfo,path
        '';
        "ghostty/dropdown".text = ''
          config-file = /home/${user}/.config/ghostty/config
          class = com.mitchellh.ghostty.dropdown
          title = dropdown
          gtk-single-instance = false
          window-decoration = none
        '';
      };
      # The T3 Code server that phones pair with over the tailnet; the firewall
      # admits it only on tailscale0.
      systemd.user.services.t3code-mobile = {
        Unit = {
          Description = "T3 Code server for mobile over Tailscale";
          StartLimitIntervalSec = 0;
        };
        Service = {
          ExecStart = "${t3Package}/bin/t3 serve --host 0.0.0.0 --port 3773";
          WorkingDirectory = "%h";
          Restart = "on-failure";
          RestartSec = 5;
        };
        Install.WantedBy = ["default.target"];
      };
      # The remote session starts from boot without a PAM login, so nothing
      # unlocks the keyring. The login keyring has an empty password (the disk is
      # unencrypted, so a stored one would add nothing), which unlocks without a
      # prompt but still starts locked, and some apps give up on a locked keyring.
      systemd.user.services.unlock-login-keyring = {
        Unit.Description = "Unlock the passwordless login keyring";
        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.systemd}/bin/busctl --user call org.freedesktop.secrets /org/freedesktop/secrets org.freedesktop.Secret.Service Unlock ao 1 /org/freedesktop/secrets/collection/login";
        };
        Install.WantedBy = ["default.target"];
      };
      services.network-manager-applet.enable = true;
      services.blueman-applet.enable = true;
      services.hyprpolkitagent.enable = true;
      home.sessionVariables = {
        NIXOS_OZONE_WL = "1";
        YDOTOOL_SOCKET = "/run/ydotoold/socket";
      };
    };
  };
}
