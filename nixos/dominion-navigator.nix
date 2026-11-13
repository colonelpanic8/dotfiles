{
  config,
  lib,
  pkgs,
  ...
}: let
  home = config.users.users.imalison.home;
  # Starts the Dominion Navigator launcher (its own Chrome, with the extension and the agent
  # launcher) as a user unit, or opens another dominion.games tab in it when it is already up.
  dominion = pkgs.writeShellApplication {
    name = "dominion";
    runtimeInputs = [pkgs.nodejs pkgs.systemd];
    text = ''
      repo="''${DOMINION_NAVIGATOR_REPO:-$HOME/projects/dominion-navigator}"
      unit=dominion-navigator
      if [ ! -d "$repo" ]; then
        echo "dominion: no dominion-navigator checkout at $repo (set DOMINION_NAVIGATOR_REPO)" >&2
        exit 1
      fi
      cd "$repo"
      if systemctl --user --quiet is-active "$unit.service"; then
        exec node --import tsx scripts/launch-debug-chrome.mjs
      fi
      npm run --silent build >/dev/null
      systemctl --user reset-failed "$unit.service" 2>/dev/null || true
      env_args=(--setenv=PATH="$PATH")
      for name in WAYLAND_DISPLAY DISPLAY XAUTHORITY; do
        if [ -n "''${!name:-}" ]; then env_args+=("--setenv=$name=''${!name}"); fi
      done
      exec systemd-run --user --unit="$unit" --collect --quiet \
        --working-directory="$repo" "''${env_args[@]}" \
        node --import tsx scripts/launch-debug-chrome.mjs
    '';
  };
in {
  home-manager.users.imalison = lib.mkIf config.myUsers.imalison.has.dotfiles {
    home.packages = [dominion];

    xdg.configFile."dominion-navigator/config.json".text = builtins.toJSON {
      agent.cwd = "${home}/projects/dominion-playbook";
    };

    xdg.desktopEntries.dominion = {
      name = "Dominion";
      comment = "Dominion Online with Dominion Navigator and its agents";
      icon = "${home}/projects/dominion-navigator/icons/icon-128.png";
      terminal = false;
      type = "Application";
      categories = ["Game"];
      exec = "${dominion}/bin/dominion";
    };
  };
}
