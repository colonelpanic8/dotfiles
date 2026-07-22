{
  inputs,
  config,
  pkgs,
  lib,
  makeEnable,
  ...
}: let
  cfg = config.myModules.desktop;
  isFull = cfg.profile == "full";
  weztermNightly = inputs.wezterm-nightly.packages.${pkgs.stdenv.hostPlatform.system}.default;
  desktopShellUi = pkgs.writeShellApplication {
    name = "desktop_shell_ui";
    runtimeInputs = [
      pkgs.bash
      pkgs.coreutils
      pkgs.systemd
    ];
    text = ''
      exec ${../dotfiles/lib/bin/desktop_shell_ui} "$@"
    '';
  };
  chromeCommandLineFlags =
    [
      "--disable-features=WaylandFractionalScaleV1"
    ]
    ++ lib.optionals config.myModules.chrome-favicon-dbus.enable [
      "--load-extension=${inputs.chrome-favicon-dbus}/extension"
    ];
  googleChromeWrapperArgs = lib.concatMapStringsSep " " (flag: "--add-flags ${lib.escapeShellArg flag}") chromeCommandLineFlags;
  googleChromeCommandWrappers = pkgs.runCommand "google-chrome-command-wrappers" {nativeBuildInputs = [pkgs.makeWrapper];} ''
    mkdir -p "$out/bin"
    makeWrapper ${pkgs.google-chrome}/bin/google-chrome "$out/bin/google-chrome" \
      ${googleChromeWrapperArgs}
    makeWrapper ${pkgs.google-chrome}/bin/google-chrome-stable "$out/bin/google-chrome-stable" \
      ${googleChromeWrapperArgs}
  '';
  googleChromeProfileWindow = pkgs.writeShellApplication {
    name = "google-chrome-profile-window";
    runtimeInputs = [
      googleChromeCommandWrappers
      pkgs.gawk
      pkgs.jq
      pkgs.rofi
    ];
    text = ''
      if [ "$#" -gt 0 ]; then
        exec google-chrome-stable "$@"
      fi

      local_state="''${CHROME_USER_DATA_DIR:-$HOME/.config/google-chrome}/Local State"

      if [ ! -r "$local_state" ]; then
        exec google-chrome-stable --new-window
      fi

      profiles="$(
        jq -r '
          (.profile.info_cache // {})
          | to_entries
          | sort_by(if .key == "Default" then 0 else 1 end, -(.value.active_time // 0))[]
          | [.value.name, .value.user_name, .key]
          | @tsv
        ' "$local_state" \
          | awk -F '\t' '{
              label = $1
              if ($2 != "") {
                label = label "  <" $2 ">"
              }
              print label "\t" $3
            }'
      )"

      if [ -z "$profiles" ]; then
        exec google-chrome-stable --new-window
      fi

      selection="$(printf '%s\n' "$profiles" | rofi -dmenu -i -p 'Chrome profile' || true)"
      if [ -z "$selection" ]; then
        exit 0
      fi

      profile_dir="$(printf '%s\n' "$selection" | awk -F '\t' '{print $NF}')"
      if [ -z "$profile_dir" ]; then
        exit 0
      fi

      exec google-chrome-stable --profile-directory="$profile_dir" --new-window
    '';
  };
  xComPwa = pkgs.writeShellApplication {
    name = "x-com-pwa";
    runtimeInputs = [
      googleChromeCommandWrappers
      pkgs.jq
    ];
    text = ''
      profile_args=()
      local_state="''${CHROME_USER_DATA_DIR:-$HOME/.config/google-chrome}/Local State"

      if [ -r "$local_state" ]; then
        profile_dir="$(
          jq -r '
            (.profile.info_cache // {})
            | to_entries
            | sort_by(if .key == "Default" then 0 else 1 end, -(.value.active_time // 0))
            | .[0].key // empty
          ' "$local_state" 2>/dev/null || true
        )"

        if [ -n "$profile_dir" ]; then
          profile_args+=(--profile-directory="$profile_dir")
        fi
      fi

      exec google-chrome-stable "''${profile_args[@]}" --class=x-com-pwa --app=https://x.com/
    '';
  };
  googleChromeDesktopEntries = pkgs.runCommand "google-chrome-desktop-entries" {nativeBuildInputs = [pkgs.gnused];} ''
    mkdir -p "$out/share/applications"

    for desktop_name in google-chrome.desktop com.google.Chrome.desktop; do
      source_file="${pkgs.google-chrome}/share/applications/$desktop_name"
      if [ -f "$source_file" ]; then
        desktop_file="$out/share/applications/$desktop_name"
        cp "$source_file" "$desktop_file"
        chmod u+w "$desktop_file"

        substituteInPlace "$desktop_file" \
          --replace-fail "${pkgs.google-chrome}/bin/google-chrome-stable" "google-chrome-stable"

        ${pkgs.gnused}/bin/sed -i \
          -e 's,application/pdf;,,g' \
          -e 's,image/gif;,,g' \
          -e 's,image/jpeg;,,g' \
          -e 's,image/png;,,g' \
          -e 's,image/webp;,,g' \
          "$desktop_file"

        ${pkgs.gnused}/bin/sed -i \
          -e 's#^Exec=.*google-chrome-stable *%U$#Exec=google-chrome-profile-window %U#' \
          -e '/^\[Desktop Action new-window\]/,/^\[Desktop Action / s#^Exec=.*google-chrome-stable.*$#Exec=google-chrome-profile-window#' \
          "$desktop_file"
      fi
    done
  '';
  spotifyWaylandFlags = [
    "--enable-features=UseOzonePlatform,WaylandWindowDecorations"
    "--ozone-platform=wayland"
    "--enable-wayland-ime=true"
  ];
  spotifyWaylandWrapperArgs = lib.concatMapStringsSep " " (flag: "--add-flags ${lib.escapeShellArg flag}") spotifyWaylandFlags;
  spotifyWaylandPatch = lib.hiPrio (pkgs.runCommand "${pkgs.spotify.name}-wayland-patch" {
      nativeBuildInputs = [
        pkgs.gnused
        pkgs.makeWrapper
      ];
    } ''
      mkdir -p "$out/bin" "$out/share/applications"

      # The packaged launcher removes DISPLAY when this is set. Spotify's
      # renderer otherwise falls back to X11 even with the Ozone flags below.
      makeWrapper ${pkgs.spotify}/bin/spotify "$out/bin/spotify" \
        --set NIXOS_OZONE_WL 1 \
        ${spotifyWaylandWrapperArgs}

      cp ${pkgs.spotify}/share/applications/spotify.desktop "$out/share/applications/spotify.desktop"
      chmod u+w "$out/share/applications/spotify.desktop"

      ${pkgs.gnused}/bin/sed -i \
        -e "s#^TryExec=.*spotify\$#TryExec=$out/bin/spotify#" \
        -e "s#^Exec=.*spotify\\( .*\\)\\?\$#Exec=$out/bin/spotify\\1#" \
        "$out/share/applications/spotify.desktop"
    '');
  vlc4Unwrapped = pkgs.callPackage ./packages/vlc4 { };
  vlc4 = pkgs.symlinkJoin {
    name = "${vlc4Unwrapped.name}-wayland";
    paths = [vlc4Unwrapped];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      wrapProgram "$out/bin/vlc" --set QT_QPA_PLATFORM wayland

      rm "$out/share/applications/vlc.desktop"
      cp ${vlc4Unwrapped}/share/applications/vlc.desktop "$out/share/applications/vlc.desktop"
      chmod u+w "$out/share/applications/vlc.desktop"
      substituteInPlace "$out/share/applications/vlc.desktop" \
        --replace-fail "${vlc4Unwrapped}/bin/vlc" "$out/bin/vlc"
    '';
  };
  rlruPackages = inputs.rlru.packages.${pkgs.stdenv.hostPlatform.system};
  scrobbleScrubber = inputs.lastfm-edit.packages.${pkgs.stdenv.hostPlatform.system}.scrobble-scrubber;
  scrobbleScrubberApp = inputs.lastfm-edit.packages.${pkgs.stdenv.hostPlatform.system}.scrobble-scrubber-app;
  rlruDioxusDesktopBase = rlruPackages.rlru-dioxus-desktop.overrideAttrs (_: {
    # Rust 1.95 can otherwise ICE/SEGV while compiling rlru's desktop dependency
    # graph in release mode.
    RUST_MIN_STACK = "2147483648";
  });
  rlruDioxusDesktop = pkgs.symlinkJoin {
    name = "${rlruDioxusDesktopBase.name}-single-desktop-entry";
    paths = [rlruDioxusDesktopBase];
    postBuild = ''
      rm -f "$out/share/applications/rlru-dioxus.desktop"
    '';
    meta = rlruDioxusDesktopBase.meta;
  };
  enabledModule = makeEnable config "myModules.desktop" true {
    services.greenclip.enable = true;
    imports = [
      ./fonts.nix
      ./hyprland.nix
      ./keyd.nix
      ./mova-capture.nix
      ./wlsunset.nix
    ];

    assertions = [
      {
        assertion = !(config.myModules.taffybar.enable && config.myModules.waybar.enable);
        message = "myModules.taffybar and myModules.waybar cannot both be enabled.";
      }
    ];

    services.xserver = {
      exportConfiguration = true;
      enable = true;
      displayManager = {
        sessionCommands = ''
          systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
        '';
      };
    };

    environment.sessionVariables = {
      # This is for the benefit of VSCODE running natively in wayland
      NIXOS_OZONE_WL = "1";
      # Claude Desktop's launcher (claude-desktop-debian flake) ignores
      # NIXOS_OZONE_WL/ELECTRON_OZONE_PLATFORM_HINT and hardcodes
      # --ozone-platform=x11 by default. This is the only knob it honors;
      # it switches the launcher to native Wayland (loses global hotkeys).
      CLAUDE_USE_WAYLAND = "1";
      IM_HYPRLAND_SHELL_UI = cfg.shellUi;
    };

    system.activationScripts.playwrightChromeCompat.text = lib.optionalString (pkgs.stdenv.hostPlatform.system == "x86_64-linux") ''
      # Playwright's Chrome channel lookup expects the FHS path below.
      mkdir -p /opt/google/chrome
      ln -sfn ${googleChromeCommandWrappers}/bin/google-chrome-stable /opt/google/chrome/chrome
    '';

    services.gnome.at-spi2-core.enable = true;

    services.gnome.gnome-keyring.enable = true;

    home-manager.users.imalison = lib.mkIf isFull {
      imports = [
        inputs.rlru.homeManagerModules.default
      ];

      services.rlru = {
        enable = true;
        package = rlruDioxusDesktop;
        startInTray = true;
      };
    };

    home-manager.sharedModules = [
      {
        imports = [./dunst.nix];

        xdg.desktopEntries."com.mitchellh.ghostty" = {
          name = "Ghostty";
          comment = "A terminal emulator";
          icon = "com.mitchellh.ghostty";
          terminal = false;
          type = "Application";
          categories = ["System" "TerminalEmulator"];
          startupNotify = true;
          exec = "${pkgs.ghostty}/bin/ghostty --gtk-single-instance=false";
          settings = {
            StartupWMClass = "com.mitchellh.ghostty";
            X-GNOME-UsesNotifications = "true";
            X-TerminalArgExec = "-e";
            X-TerminalArgTitle = "--title=";
            X-TerminalArgAppId = "--class=";
            X-TerminalArgDir = "--working-directory=";
            X-TerminalArgHold = "--wait-after-command";
          };
          actions = {
            new-window = {
              name = "New Window";
              exec = "${pkgs.ghostty}/bin/ghostty --gtk-single-instance=false";
            };
          };
        };

        xdg.desktopEntries.x-com-pwa = lib.mkIf isFull {
          name = "X";
          genericName = "Social Network";
          comment = "Open x.com in a dedicated Chrome app window";
          icon = "google-chrome";
          terminal = false;
          type = "Application";
          categories = ["Network"];
          startupNotify = true;
          exec = "${xComPwa}/bin/x-com-pwa";
          settings = {
            StartupWMClass = "x-com-pwa";
          };
        };

        xdg.configFile."ghostty/config" = {
          force = true;
          text = ''
            confirm-close-surface = false
            shell-integration-features = cursor,no-sudo,no-title,no-ssh-env,no-ssh-terminfo,path
          '';
        };

        xdg.configFile."ghostty/dropdown" = {
          force = true;
          text = ''
            config-file = /home/imalison/.config/ghostty/config
            class = com.mitchellh.ghostty.dropdown
            title = dropdown
            gtk-single-instance = false
            window-decoration = none
            window-padding-x = 0
            window-padding-y = 0
            background-opacity = 0.82
            background-opacity-cells = true
            background-blur = false
          '';
        };

        systemd.user.services.hyprpolkitagent = {
          Unit = {
            Description = "Graphical PolicyKit authentication agent";
            After = ["graphical-session.target"];
            PartOf = ["graphical-session.target"];
          };
          Service = {
            ExecStart = lib.getExe pkgs.hyprpolkitagent;
            Restart = "on-failure";
            RestartSec = 2;
          };
          Install.WantedBy = ["graphical-session.target"];
        };
      }
    ];

    environment.systemPackages =
      (with pkgs; [
        desktopShellUi

        # Lightweight personal desktop baseline.
        adwaita-icon-theme
        hicolor-icon-theme
        kdePackages.qt6ct
        numix-icon-theme-circle
        papirus-icon-theme
        keyd
        ghostty
        file-roller
        libnotify
        localsend
        lxappearance
        networkmanagerapplet
        pinentry-gnome3
        rofi
        wofi
        rofi-systemd
        thunar
        pavucontrol
        playerctl
        pulsemixer
        brightnessctl
      ])
      ++ lib.optionals isFull (with pkgs; [
        libsForQt5.qt5ct
        autorandr
        wmctrl
        xclip
        xdotool
        xev
        xwininfo
        xsettingsd
        alacritty
        weztermNightly
        blueman
        d-spy
        kdePackages.dolphin
        feh
        gthumb
        firefox
        cheese
        kdePackages.kleopatra
        libreoffice
        loupe
        lxqt.lxqt-powermanagement
        kdePackages.okular
        quassel
        remmina
        rofi-pass
        scrobbleScrubberApp
        scrobbleScrubber
        simplescreenrecorder
        skippy-xd
        transmission_4-gtk
        vlc4
        thunar
        tumbler

        # Audio
        picard
        espeak
        graphviz
        mermaid-cli
        pandoc
      ])
      ++ lib.optionals (pkgs.stdenv.hostPlatform.system == "x86_64-linux") [
        googleChromeCommandWrappers
        googleChromeDesktopEntries
        googleChromeProfileWindow
      ]
      ++ lib.optionals (isFull && pkgs.stdenv.hostPlatform.system == "x86_64-linux") (with pkgs; [
        pommed_light
        slack
        spicetify-cli
        spotify
        spotifyWaylandPatch
        tor-browser
        xComPwa
        zulip
      ]);
  };
in
  enabledModule
  // {
    options = lib.recursiveUpdate enabledModule.options {
      myModules.desktop.shellUi = lib.mkOption {
        type = lib.types.enum ["taffybar"];
        default = "taffybar";
        description = ''
          Desktop shell UI used by Hyprland-oriented bindings. This controls
          the active shell service and Hyprland launcher/window picker dispatch.
        '';
      };
      myModules.desktop.profile = lib.mkOption {
        type = lib.types.enum ["minimal" "full"];
        default = "full";
        description = ''
          Select the lightweight personal session baseline or the complete
          workstation application set.
        '';
      };
    };
  }
