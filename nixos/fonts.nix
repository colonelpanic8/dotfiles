{ pkgs, ... }:
{
  # Enable the gtk icon cache
  gtk.iconCache.enable = true;

  fonts = {
    fonts = with pkgs; [
      dejavu_fonts
      emojione
      fira-mono
      font-awesome-ttf
      noto-fonts-emoji
      roboto
      source-code-pro
      source-sans-pro
      source-serif-pro
      twemoji-color-font
      emacs-all-the-icons-fonts
    ];
    fontconfig = {
      allowBitmaps = true;
      useEmbeddedBitmaps = true;
      defaultFonts = {
        monospace = [ "Source Code Pro" ];
        sansSerif = [ "Roboto" ];
        serif     = [ "Source Serif Pro" ];
      };
    };
  };
}
