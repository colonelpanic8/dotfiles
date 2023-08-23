{ pkgs, config, specialArgs, ... }:
{
  services.emacs = {
    enable = false;
    defaultEditor = true;
    startWithUserSession = "graphical";
    socketActivation.enable = true;
  };
}
