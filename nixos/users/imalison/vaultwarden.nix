{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  desktop = config.myModules.desktop.enable;
  rbw = inputs.rbw-passkey.packages.${pkgs.stdenv.hostPlatform.system}.default;
  picker = (pkgs.rofi-rbw.override {inherit rbw;}).overrideAttrs (old: {
    patches = (old.patches or []) ++ [../../patches/rofi-rbw-action-menu.patch];
  });
  selector = pkgs.writeShellApplication {
    name = "rofi";
    runtimeInputs = [pkgs.bash pkgs.rofi pkgs.systemd];
    text = ''
      exec ${../../../dotfiles/lib/bin/desktop_dmenu} "$@"
    '';
  };
  menu = pkgs.writeShellApplication {
    name = "vaultwarden-menu";
    runtimeInputs = [selector rbw pkgs.libnotify pkgs.wl-clipboard pkgs.wtype pkgs.xclip pkgs.xdotool];
    text = ''
      exec ${lib.getExe picker} "$@"
    '';
  };
  desktopEntry = pkgs.makeDesktopItem {
    name = "vaultwarden-menu";
    desktopName = "Vaultwarden Passwords";
    comment = "Search and copy passwords from Vaultwarden";
    exec = lib.getExe menu;
    icon = "bitwarden";
    terminal = false;
    categories = ["Utility"];
  };
in {
  config = lib.mkIf config.myUsers.imalison.has.full {
    system.build.vaultwarden-menu = lib.mkIf desktop menu;

    home-manager.users.imalison = {
      config,
      lib,
      ...
    }: {
      programs.rbw = {
        enable = true;
        package = rbw;
        settings = {
          email = "ivanmalison@gmail.com";
          base_url = "https://jimi-hendnix.taileb3aad.ts.net:8222";
          login_method = "passkey";
          lock_timeout = 900;
          pinentry =
            if desktop
            then pkgs.pinentry-qt
            else pkgs.pinentry-curses;
        };
      };

      home.file."${config.xdg.configHome}/rbw/config.json".force = true;
      home.packages = lib.optionals desktop [menu];

      xdg.configFile."rofi-rbw.rc" = lib.mkIf desktop {
        force = true;
        text = ''
          selector=rofi
          action=type
          target=menu
          action-menu=true
          clear-after=30
          prompt=Vaultwarden
          selector-args=-disable-history
          use-notify-send=true
          keybindings=Alt+a:type:username:tab:password,Alt+p:type:password,Alt+u:type:username,Alt+2:copy:password,Alt+0:copy:username,Alt+t:copy:totp,Alt+1::menu,Alt+s:sync
          menu-keybindings=Alt+t:type,Alt+c:copy
        '';
      };

      systemd.user.services.rbw-passkey-agent = lib.mkIf desktop {
        Unit = {
          Description = "Bitwarden agent with YubiKey passkey support";
          PartOf = ["graphical-session.target"];
        };
        Service = {
          ExecStart = "${rbw}/bin/rbw-agent --no-daemonize";
          Restart = "on-failure";
          RestartSec = 2;
          UMask = "0077";
        };
        Install.WantedBy = ["graphical-session.target"];
      };
      xdg.configFile."systemd/user/rbw-passkey-agent.service" = lib.mkIf desktop {force = true;};

      xdg.dataFile."applications/vaultwarden-menu.desktop" = lib.mkIf desktop {
        source = "${desktopEntry}/share/applications/vaultwarden-menu.desktop";
        force = true;
      };

      home.activation.removeImperativeRbw = lib.hm.dag.entryAfter ["installPackages" "linkGeneration"] ''
        if profile_json="$(${pkgs.nix}/bin/nix profile list --json 2>/dev/null)"; then
          for package in rbw launcher; do
            if printf '%s' "$profile_json" | ${pkgs.jq}/bin/jq -e --arg name "$package" \
              '.elements[$name].originalUrl == "github:colonelpanic8/rbw/passkey-login"' >/dev/null; then
              run ${pkgs.nix}/bin/nix profile remove "$package"
            fi
          done
        fi
      '';
    };
  };
}
