self: super:

{
  rofi = super.rofi.overrideAttrs(_: rec {
    version = "1.5.3";
    src = super.fetchurl {
      url = "https://github.com/DaveDavenport/rofi/releases/download/${version}/rofi-${version}.tar.gz";
      sha256 = "1mskknfnpgmaghplwcyc44qc8swb1f9qiyi67fz9i77jijjpj1lx";
    };
  });
  clipit = super.clipit.overrideAttrs (oldAttrs: rec {
    version = "50d983514386029a1f133187902084b753458f32";
    preConfigure = "./autogen.sh";
    configureFlags = ["--with-gtk3" "--enable-appindicator"];
    src = super.fetchFromGitHub {
      owner = "IvanMalison";
      repo = "ClipIt";
      sha256 = "1d52zjnxmcp2kr4wvq2yn9fhr61v9scp91fxfvasvz5m7k1zagdn";
      rev = version;
    };
    buildInputs = with self; [
      autoconf automake intltool gtk3 xdotool hicolor-icon-theme
      libappindicator-gtk3
    ];
  });
  pasystray-appindicator = super.pasystray.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [self.libappindicator-gtk3];
  });
  customizable-notify-osd = super.notify-osd.overrideAttrs (oldAttrs: rec {
    version = "0.9.35+16.04.20160415";
    baseURI = "https://launchpad.net/~leolik/+archive/leolik";
    src = super.fetchurl {
      url = "${baseURI}/+files/notify-osd_${version}-0ubuntu1-leolik~ppa0.tar.gz";
      sha256 = "026dr46jh3xc4103wnslzy7pxbxkkpflh52c59j8vzwaa7bvvzkv";
      name = "notify-osd-customizable.tar.gz";
    };
    preConfigure = "./autogen.sh --libexecdir=$(out)/bin";
    buildInputs = with self; [
      glib libwnck3 libnotify dbus-glib gnome3.gsettings-desktop-schemas
      makeWrapper libtool gnome3.gnome-common
    ];
  });
  networkmanager_strongswan = super.networkmanager_strongswan.overrideAttrs (oldAttrs: rec {
    src = super.fetchFromGitHub {
      owner = "IvanMalison";
      repo = "NetworkManager-strongswan";
      sha256 = "0vcg58xrjacdswz2fxahgi7shgf2v14mfpscnwza6wns8qx37yzb";
      rev = "2849b1817926b7973a5dc530bed7250c95c733bf";
    };
  });
  strongswanNM = super.strongswanNM.overrideAttrs (oldAttrs: rec {
    patches = oldAttrs.patches ++ [ ./patch-strongswan.patch ];
  });

  lorri = super.lorri.overrideAttrs (_: {
    src = super.fetchFromGitHub {
      owner = "target";
      repo = "lorri";
      rev = "3e57656a536aada13eb7b33c07e0d637772d095d";
      sha256 = "1q9ddbndnda1njp8nwqklbckqxpsv2g7936b566imipmmfdb67y0";
    };
  });
}
