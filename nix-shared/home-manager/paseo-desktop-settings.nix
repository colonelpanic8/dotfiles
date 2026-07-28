{
  config,
  lib,
  pkgs,
  ...
}: let
  # The only three preferences the desktop app persists. Everything else in
  # desktop-settings.json is migration bookkeeping the app owns.
  desiredSettings = {
    releaseChannel = "stable";
    daemon = {
      manageBuiltInDaemon = true;
      keepRunningAfterQuit = false;
    };
  };
  desiredSettingsJson = builtins.toJSON desiredSettings;

  # Electron derives userData from electron-builder's `productName: Paseo`, so
  # this directory is capitalized and distinct from the lowercase ~/.config/paseo
  # that holds managed-hosts.json. See getpaseo/paseo#2522.
  settingsPath =
    if pkgs.stdenv.isDarwin
    then "${config.home.homeDirectory}/Library/Application Support/Paseo/desktop-settings.json"
    else "${config.xdg.configHome}/Paseo/desktop-settings.json";

  # The app rewrites this file on every load and on every settings change
  # (atomic temp file + rename), so it cannot be a Home Manager symlink: the
  # rename would replace the link with a regular file. Render a real file and
  # merge, preserving the app's `migrations` block so migrations do not re-run.
  renderSettings = pkgs.writeShellScript "render-paseo-desktop-settings" ''
    set -eu

    settings_path=${lib.escapeShellArg settingsPath}
    settings_dir=${lib.escapeShellArg (builtins.dirOf settingsPath)}
    mkdir -p "$settings_dir"

    existing='{}'
    if [ -f "$settings_path" ]; then
      existing="$(${pkgs.jq}/bin/jq '.' "$settings_path" 2>/dev/null || echo '{}')"
    fi

    temporary="$(${pkgs.coreutils}/bin/mktemp "$settings_dir/.desktop-settings.json.XXXXXX")"
    trap 'rm -f "$temporary"' EXIT

    ${pkgs.jq}/bin/jq -n \
      --argjson existing "$existing" \
      --argjson desired ${lib.escapeShellArg desiredSettingsJson} \
      '$existing
       | .version = 1
       | .settings = ((.settings // {}) * $desired)
       # daemonStopOnQuitDefaultApplied must stay true, otherwise the app
       # overwrites keepRunningAfterQuit with its own default on next load.
       | .migrations = ({legacyRendererSettingsImported: false}
                        * (.migrations // {})
                        * {daemonStopOnQuitDefaultApplied: true})' \
      > "$temporary"

    chmod 0644 "$temporary"
    mv "$temporary" "$settings_path"
    trap - EXIT
  '';
in
  lib.mkIf (config.home.username == "imalison") {
    systemd.user.services.paseo-desktop-settings = lib.mkIf pkgs.stdenv.isLinux {
      Unit.Description = "Render declarative Paseo desktop settings";
      Install.WantedBy = ["default.target"];
      Service = {
        Type = "oneshot";
        ExecStart = "${renderSettings}";
      };
    };

    launchd.agents.paseo-desktop-settings = lib.mkIf pkgs.stdenv.isDarwin {
      enable = true;
      config = {
        ProgramArguments = ["${renderSettings}"];
        RunAtLoad = true;
        StandardOutPath = "${config.home.homeDirectory}/Library/Logs/paseo-desktop-settings.log";
        StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/paseo-desktop-settings.err.log";
      };
    };
  }
