self: super:

let
  lorriSource = (import <nixpkgs> {}).fetchurl {
    url = "https://raw.githubusercontent.com/target/lorri/master/direnv/nixos.nix";
    sha256 = "057kqbivf4xbhakz1j1b19sxd5c6p6rqhg6pwnq2zfvvmp8nmylm";
  };
  lorriBinSource = super.fetchFromGitHub {
    owner = "IvanMalison";
    repo = "lorri";
    rev = "94c7152745688c1268262a42d1d0fa0b7b2f6309";
    sha256 = "07mcmj44f35yb2ifzr4zmazqv54dh0vhvkdikbymm2vdmjh8xdw8";
  };
in
{
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
  lorri = (import (lorriBinSource.outPath + "/default.nix")) { pkgs = super; };
}
