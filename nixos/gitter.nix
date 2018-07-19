{ stdenv, alsaLib, atk, cairo, cups, dbus, dpkg, expat, fetchurl
, fontconfig, freetype, gdk_pixbuf, glib, gnome3, gtk3, libX11
, libXScrnSaver, libXcomposite, libXcursor, libXdamage, libXext, libXfixes
, libXi, libXrandr, libXrender, libXtst, libappindicator-gtk3, libcxx
, libnotify, libpulseaudio, libxcb, makeDesktopItem, makeWrapper, nspr, nss
, nwjs, pango, systemd }:

let gitterDirectorySuffix = "opt/gitter";
    doELFExePatch = target: ''
      patchelf --set-interpreter ${stdenv.cc.bintools.dynamicLinker} \
         --set-rpath "$out/${gitterDirectorySuffix}/lib:${libPath}" \
         $out/${gitterDirectorySuffix}/${target}
       '';
   doELFLibPatch = target: ''
      patchelf --set-rpath "$out/${gitterDirectorySuffix}/lib:${libPath}" \
         $out/${gitterDirectorySuffix}/${target}
       '';
   libPath = stdenv.lib.makeLibraryPath [
     alsaLib atk cairo cups dbus expat fontconfig freetype gdk_pixbuf glib
     gnome3.gconf gtk3 libX11 libXScrnSaver libXcomposite libXcursor libXdamage
     libXext libXfixes libXi libXrandr libXrender libXtst libappindicator-gtk3
     libcxx libnotify libpulseaudio libxcb nspr nss pango stdenv.cc.cc systemd
  ];
in stdenv.mkDerivation rec {
  pname = "gitter";
  version = "4.1.0";
  name = "${pname}-${version}";

  src = fetchurl {
    url = "https://update.gitter.im/linux64/${pname}_${version}_amd64.deb";
    sha256 = "1gny9i2pywvczzrs93k8krqn6hwm6c2zg8yr3xmjqs3p88817wbi";
  };

  nativeBuildInputs = [ makeWrapper dpkg ];

  unpackPhase = "dpkg -x $src .";

  installPhase = ''
    mkdir -p $out/{bin,opt/gitter,share/pixmaps}
    mv ./opt/Gitter/linux64/* $out/opt/gitter

    ${doELFExePatch "Gitter"}
    ${doELFExePatch "nacl_helper"}
    ${doELFExePatch "minidump_stackwalk"}
    ${doELFExePatch "nwjc"}
    ${doELFExePatch "chromedriver"}
    ${doELFExePatch "payload"}

    ${doELFLibPatch "lib/libnw.so"}
    ${doELFLibPatch "lib/libnode.so"}
    ${doELFLibPatch "lib/libffmpeg.so"}

    wrapProgram $out/${gitterDirectorySuffix}/Gitter --prefix LD_LIBRARY_PATH : ${libPath}

    ln -s $out/${gitterDirectorySuffix}/Gitter $out/bin/
    ln -s $out/${gitterDirectorySuffix}/logo.png $out/share/pixmaps/gitter.png
    ln -s "${desktopItem}/share/applications" $out/share/
  '';

  desktopItem = makeDesktopItem {
    name = pname;
    exec = "/usr/bin/env Gitter";
    icon = pname;
    desktopName = "Gitter";
    genericName = meta.description;
    categories = "Network;InstantMessaging;";
  };

  meta = with stdenv.lib; {
    description = "Where developers come to talk";
    downloadPage = "https://gitter.im/apps";
    license = licenses.mit;
    maintainers = [ maintainers.imalison ];
    platforms = [ "x86_64-linux" ];
  };
}
