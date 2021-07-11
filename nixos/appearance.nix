{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Appearance
    gnome-breeze
    gnome3.adwaita-icon-theme
    hicolor-icon-theme
    materia-theme
    numix-icon-theme-circle
    papirus-icon-theme
  ];
}
