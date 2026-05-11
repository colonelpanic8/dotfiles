{
  pkgs,
  config,
  makeEnable,
  ...
}: let
  discordWayland = pkgs.discord.override {
    commandLineArgs = "--enable-features=WaylandWindowDecorations,WebRTCPipeWireCapturer --enable-wayland-ime=true";
  };
in
  makeEnable config "myModules.electron" false {
    environment.systemPackages = with pkgs; [
      element-desktop
      # bitwarden
      discordWayland
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
