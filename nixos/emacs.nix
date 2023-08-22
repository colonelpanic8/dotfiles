{ pkgs, config, specialArgs, ... }:
{
  services.emacs = {
    enable = true;
    defaultEditor = true;
    startWithUserSession = "graphical";
    socketActivation.enable = true;
  };
}
