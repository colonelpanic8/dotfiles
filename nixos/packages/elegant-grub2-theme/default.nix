{
  fetchFromGitHub,
  imagemagick,
  lib,
  stdenvNoCC,
  theme ? "wave",
  style ? "window",
  side ? "left",
  color ? "dark",
  screenVariant ? "1080p",
  width ? null,
  height ? null,
}: let
  customResolution = width != null && height != null;
  fontSizes = {
    "1080p" = "16";
    "2k" = "24";
    "4k" = "32";
  };
  fontSize = builtins.getAttr screenVariant fontSizes;
  themeName =
    "Elegant-${theme}-${style}-${side}-${color}"
    + lib.optionalString customResolution "-${width}x${height}";
in
  stdenvNoCC.mkDerivation {
    pname = "elegant-grub2-theme";
    version = "2025-03-25";

    src = fetchFromGitHub {
      owner = "vinceliuice";
      repo = "Elegant-grub2-themes";
      rev = "2025-03-25";
      hash = "sha256-M9k6R/rUvEpBTSnZ2PMv5piV50rGTBrcmPU4gsS7Byg=";
    };

    nativeBuildInputs = lib.optional customResolution imagemagick;

    installPhase =
      ''
        runHook preInstall

        theme_dir="$out/share/grub/themes/${themeName}"
        mkdir -p "$theme_dir"

        cp -a common/terminus*.pf2 "$theme_dir"/
        cp -a common/unifont-${fontSize}.pf2 "$theme_dir"/
        cp -a assets/assets-icons-${color}/icons-${color}-${screenVariant} "$theme_dir/icons"
        cp -a config/theme-${style}-${side}-${color}-${screenVariant}.txt "$theme_dir/theme.txt"
        cp -a assets/assets-other/other-${screenVariant}/select_e-${theme}-${color}.png "$theme_dir/select_e.png"
        cp -a assets/assets-other/other-${screenVariant}/select_c-${theme}-${color}.png "$theme_dir/select_c.png"
        cp -a assets/assets-other/other-${screenVariant}/select_w-${theme}-${color}.png "$theme_dir/select_w.png"
        cp -a assets/assets-other/other-${screenVariant}/Default.png "$theme_dir/logo.png"
      ''
      + lib.optionalString customResolution ''
        magick backgrounds/backgrounds-${theme}/background-${theme}-${style}-${side}-${color}.jpg \
          -resize ${width}x${height}^ \
          -gravity center \
          -extent ${width}x${height} \
          "$theme_dir/background.jpg"
        magick assets/assets-other/other-${screenVariant}/${style}-${side}.png \
          -resize x${height} \
          -background none \
          -gravity west \
          -extent ${width}x${height} \
          "$theme_dir/info.png"
      ''
      + lib.optionalString (!customResolution) ''
        cp -a backgrounds/backgrounds-${theme}/background-${theme}-${style}-${side}-${color}.jpg "$theme_dir/background.jpg"
        cp -a assets/assets-other/other-${screenVariant}/${style}-${side}.png "$theme_dir/info.png"
      ''
      + ''

        runHook postInstall
      '';

    passthru = {
      inherit themeName;
    };

    meta = {
      description = "Elegant wave GRUB2 theme";
      homepage = "https://github.com/vinceliuice/Elegant-grub2-themes";
      license = lib.licenses.gpl3Only;
      maintainers = [];
    };
  }
