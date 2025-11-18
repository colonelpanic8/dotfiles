{ pkgs, makeEnable, config, ... }:
makeEnable config "myModules.fonts" true {
  # Enable the gtk icon cache
  gtk.iconCache.enable = true;

  nixpkgs.config.joypixels.acceptLicense = true;

  fonts = {
    packages = with pkgs; [
      cascadia-code
      dejavu_fonts
      emacs-all-the-icons-fonts
      joypixels
      fira-code
      fira-mono
      font-awesome

      noto-fonts-color-emoji
      roboto
      source-code-pro
      source-sans-pro
      source-serif-pro
      twemoji-color-font
      nerd-fonts.jetbrains-mono
      nerd-fonts.fantasque-sans-mono
      nerd-fonts.iosevka
      nerd-fonts.victor-mono
    ];
    fontconfig = {
      hinting.autohint = true;
      antialias = true;
      allowBitmaps = true;
      useEmbeddedBitmaps = true;
      defaultFonts = {
        monospace = [ "JetBrains Mono" ];
        sansSerif = [ "Roboto" ];
        serif     = [ "Source Serif Pro" ];
      };
    };
  };
}
