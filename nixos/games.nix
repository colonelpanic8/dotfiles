{
  config,
  lib,
  pkgs,
  makeEnable,
  ...
}: let
  repairXwaylandSocket = pkgs.writeShellApplication {
    name = "repair-xwayland-socket";
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
  steamWithXwaylandSocketRepair = pkgs.symlinkJoin {
    name = "steam-with-xwayland-socket-repair";
    paths = [pkgs.steam];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      rm -f "$out/bin/steam"
      makeWrapper ${pkgs.steam}/bin/steam "$out/bin/steam" \
        --run ${lib.escapeShellArg "${repairXwaylandSocket}/bin/repair-xwayland-socket"}
    '';
  };
in
  makeEnable config "myModules.games" false {
    environment.systemPackages = with pkgs; [
      repairXwaylandSocket
      steamWithXwaylandSocketRepair
      heroic
    ];
    hardware.xone.enable = true;
  }
