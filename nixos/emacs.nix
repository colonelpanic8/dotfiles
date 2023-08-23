{ pkgs, config, specialArgs, ... }:
{
  services.emacs = {
    enable = true;
    defaultEditor = true;
    startWithUserSession = false;
    socketActivation.enable = true;
  };
}
