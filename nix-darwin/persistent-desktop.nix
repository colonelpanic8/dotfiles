{config, ...}: let
  primaryUser = config.system.primaryUser;
in {
  system.defaults.CustomUserPreferences."com.apple.screensaver".idleTime = 0;
  system.defaults.screensaver.askForPassword = false;
  system.defaults.screensaver.askForPasswordDelay = 0;

  system.activationScripts.postActivation.text = ''
    echo >&2 "current-host screensaver defaults..."
    primary_uid="$(id -u -- ${primaryUser})"
    if launchctl print "gui/$primary_uid" >/dev/null 2>&1; then
      launchctl asuser "$primary_uid" sudo --user=${primaryUser} -- defaults -currentHost write com.apple.screensaver askForPassword -bool false
      launchctl asuser "$primary_uid" sudo --user=${primaryUser} -- defaults -currentHost write com.apple.screensaver askForPasswordDelay -int 0
      launchctl asuser "$primary_uid" sudo --user=${primaryUser} -- defaults -currentHost write com.apple.screensaver idleTime -int 0
    else
      echo >&2 "skipping current-host screensaver defaults; ${primaryUser} has no GUI launchd session"
    fi
  '';

  power.sleep = {
    computer = "never";
    display = "never";
    harddisk = "never";
  };
}
