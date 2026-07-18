{
  pkgs,
  config,
  makeEnable,
  ...
}: let
  repairXwaylandSocket = pkgs.writeShellApplication {
    name = "repair-xwayland-socket-for-discord";
    text = ''
      if [ -z "''${DISPLAY:-}" ]; then
        exit 0
      fi

      display_part="''${DISPLAY#*:}"
      display_number="''${display_part%%.*}"
      case "$display_number" in
        ""|*[!0-9]*) exit 0 ;;
      esac

      socket="/tmp/.X11-unix/X$display_number"
      fallback_socket="''${socket}_"

      if [ -S "$socket" ]; then
        exit 0
      fi

      if [ -L "$socket" ] && [ ! -e "$socket" ]; then
        rm -f "$socket" 2>/dev/null || true
      fi

      if [ ! -e "$socket" ] && [ -S "$fallback_socket" ]; then
        ln -s "X''${display_number}_" "$socket" 2>/dev/null || true
      fi
    '';
  };
  discordXlibThreadInitSource = pkgs.writeText "discord-xlib-thread-init.c" ''
    #include <X11/Xlib.h>

    __attribute__((constructor)) static void initialize_xlib_threads(void) {
      XInitThreads();
    }
  '';
  discordXlibThreadInit = pkgs.stdenv.mkDerivation {
    pname = "discord-xlib-thread-init";
    version = "1";
    dontUnpack = true;
    nativeBuildInputs = [pkgs.pkg-config];
    buildInputs = [pkgs.libx11];
    buildPhase = ''
      $CC -shared -fPIC ${discordXlibThreadInitSource} \
        -o libdiscord-xlib-thread-init.so \
        $(${pkgs.pkg-config}/bin/pkg-config --cflags --libs x11)
    '';
    installPhase = ''
      install -Dm755 libdiscord-xlib-thread-init.so \
        "$out/lib/libdiscord-xlib-thread-init.so"
    '';
  };
  discordPackage = pkgs.discord.override {
    commandLineArgs = "--enable-features=WaylandWindowDecorations,WebRTCPipeWireCapturer --enable-wayland-ime=true";
  };
  discordWithXwaylandSocketRepair = pkgs.symlinkJoin {
    name = "discord-with-xwayland-socket-repair";
    paths = [discordPackage];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      rm -f "$out/bin/discord"
      makeWrapper ${discordPackage}/bin/discord "$out/bin/discord" \
        --run ${repairXwaylandSocket}/bin/repair-xwayland-socket-for-discord \
        --prefix LD_PRELOAD : ${discordXlibThreadInit}/lib/libdiscord-xlib-thread-init.so
    '';
  };
in
  makeEnable config "myModules.electron" false {
    environment.systemPackages = with pkgs; [
      element-desktop
      # bitwarden
      discordWithXwaylandSocketRepair
      vesktop
      # etcher
      # keybase-gui
      zoom-us
    ];
    home-manager.sharedModules = [
      (
        if pkgs.stdenv.hostPlatform.system == "x86_64-linux"
        then {
          # systemd.user.services.bitwarden = {
          #   Unit = {
          #     Description = "Bitwarden";
          #     After = [ "graphical-session-pre.target" "tray.target" ];
          #     PartOf = [ "graphical-session.target" ];
          #   };

          #   Install = { WantedBy = [ "graphical-session.target" ]; };

          #   Service = {
          #     ExecStart = "${pkgs.bitwarden}/bin/bitwarden";
          #     Restart = "always";
          #     RestartSec = 3;
          #   };
          # };
        }
        else {}
      )
    ];
  }
