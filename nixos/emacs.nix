{ pkgs, config, specialArgs, ... }:
{
  services.emacs = {
    enable = false;
    defaultEditor = true;
    socketActivation.enable = true;
  };
}
