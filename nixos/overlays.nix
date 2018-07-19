self: super:

{
  gitter = super.callPackage ./gitter.nix { };
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
  udiskie-appindicator = super.udiskie.overrideAttrs (oldAttrs: rec {
    version = "1.7.5";
    src = super.fetchFromGitHub {
      owner = "coldfix";
      repo = "udiskie";
      rev = version;
      sha256 = "1mcdn8ha5d5nsmrzk6xnnsqrmk94rdrzym9sqm38zk5r8gpyl1k4";
    };
    propagatedBuildInputs = oldAttrs.propagatedBuildInputs ++ [self.libappindicator-gtk3];
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
  keybase-gui-fixed = super.keybase-gui.overrideAttrs (oldAttrs: rec {
    installPhase = ''
    mkdir -p $out/bin
    mv usr/share $out/share
    mv opt/keybase $out/share/
    cat > $out/bin/keybase-gui <<EOF
    #!${self.stdenv.shell}
    checkFailed() {
      if [ "\$NIX_SKIP_KEYBASE_CHECKS" = "1" ]; then
        return
      fi
      echo "Set NIX_SKIP_KEYBASE_CHECKS=1 if you want to skip this check." >&2
      exit 1
    }
    if [ ! -S "\$XDG_RUNTIME_DIR/keybase/keybased.sock" ]; then
      echo "Keybase service doesn't seem to be running." >&2
      echo "You might need to run: keybase service" >&2
      checkFailed
    fi
    if [ -z "\$(keybase status | grep kbfsfuse)" ]; then
      echo "Could not find kbfsfuse client in keybase status." >&2
      echo "You might need to run: kbfsfuse" >&2
      checkFailed
    fi
    exec $out/share/keybase/Keybase "\$@"
    EOF
    chmod +x $out/bin/keybase-gui
    substituteInPlace $out/share/applications/keybase.desktop \
      --replace run_keybase $out/bin/keybase-gui
  '';
  });
}
