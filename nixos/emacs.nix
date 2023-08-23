{ pkgs, config, specialArgs, ... }:
{
  services.emacs = {
    enable = false;
    defaultEditor = true;
    startWithUserSession = false;
    socketActivation.enable = true;
  };
}
