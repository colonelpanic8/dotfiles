{
  imports = [
    ../options.nix
    ../environment.nix
    ../display-manager.nix
    ../desktop.nix
    ../taffybar.nix
    ../sni.nix
    ../chrome-favicon-dbus.nix
    ../waybar.nix
    ../rescue-image.nix
  ];

  # A normal machine identity with a deliberately small, portable module set.
  # Do not import configuration.nix here: that would evaluate and enable many
  # workstation/server assumptions that do not belong on rescue media.
  dotfilesEnvironment.enableHostPythonCompletions = false;
  myModules.displayManager.enable = true;
  myModules.desktop = {
    enable = true;
    profile = "minimal";
  };
  myModules.fonts.enable = false;
  myModules.hyprland.portable = true;
  myModules.movaCapture.enable = false;
  myModules.taffybar.profile = "minimal";
  myModules.hostIdentity = {
    emoticon = "🛟";
    tmux.background = "#0369a1";
  };
}
