# Home Manager defaults for the account a machine belongs to
# (myModules.primaryUser), on workstation and standalone hosts alike.
# Ivan's own identity and services stay in ./imalison.nix.
{
  config,
  lib,
  ...
}: {
  home-manager.users.${config.myModules.primaryUser} = {
    lib,
    osConfig,
    pkgs,
    ...
  }: {
    # The GTK file-chooser right-click context menu (Show Hidden Files, etc.)
    # is unreliable under Hyprland/wlroots (upstream popup-focus bug,
    # hyprwm/Hyprland#6426), so set these toggles as defaults instead of
    # relying on the broken menu. Ctrl+H still toggles hidden files live.
    dconf.settings."org/gtk/settings/file-chooser" = {
      show-hidden = true;
      sort-directories-first = true;
    };

    programs.gh = {
      enable = true;
      settings.git_protocol = "ssh";
    };

    systemd.user.services.tailscale-systray = lib.mkIf osConfig.services.tailscale.enable {
      Unit = {
        Description = "Tailscale systray";
        After = ["graphical-session.target" "tray.target"];
        PartOf = ["graphical-session.target"];
        Requires = ["tray.target"];
      };

      Service = {
        ExecStart = "${pkgs.tailscale}/bin/tailscale systray";
        Restart = "on-failure";
        RestartSec = 3;
      };

      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };
  };
}
