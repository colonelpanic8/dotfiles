{ pkgs, config, specialArgs, ... }:
{
  services.emacs = {
    enable = true;
    defaultEditor = true;
    socketActivation.enable = true;
  };
}
