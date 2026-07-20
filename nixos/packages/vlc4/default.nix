{
  kdePackages,
  lib,
  vlc,
  fetchzip,
  python3,
}:

let
  stableVlc = vlc.override {
    chromecastSupport = false;
    skins2Support = false;
    withQt5 = false;
  };
  vlcQtEnv = kdePackages.env "vlc4-qt-env" [
    kdePackages.qtdeclarative
    kdePackages.qtshadertools
    kdePackages.qtsvg
    kdePackages.qtwayland
  ];
in
stableVlc.overrideAttrs (oldAttrs: rec {
  pname = "vlc";
  version = "4.0.0-dev-2026-07-20";

  src = fetchzip {
    url = "https://code.videolan.org/videolan/vlc/-/archive/76a78a0b3132a814d066b54c993a3b5cd6ed9243/vlc-76a78a0b3132a814d066b54c993a3b5cd6ed9243.tar.gz";
    hash = "sha256-z8sAwnVP7C0oq2b5V9iI51d6MHjfqboBGiTKufgE9Hg=";
  };

  patches = [ ];

  env = oldAttrs.env // {
    # qmake emits module-local include paths, but Qt's forwarding headers use
    # paths relative to the qtdeclarative include root.
    CPPFLAGS = "-I${kdePackages.qtdeclarative}/include";
  };

  postPatch = oldAttrs.postPatch + ''
    substituteInPlace modules/gui/qt/maininterface/navigationmodel.hpp \
      --replace-fail \
        '#include <QQmlEngine>' \
        $'#include <QQmlEngine>\n#include <QQmlParserStatus>'
  '';

  nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [
    kdePackages.wrapQtAppsHook
    python3
    vlcQtEnv
  ];

  buildInputs = oldAttrs.buildInputs ++ [
    kdePackages.qtbase
    kdePackages.qtdeclarative
    kdePackages.qtshadertools
    kdePackages.qtsvg
    kdePackages.qtwayland
  ];

  configureFlags =
    builtins.filter (flag: !(lib.hasPrefix "--with-kde-solid=" flag)) oldAttrs.configureFlags
    ++ [
      "--enable-qt"
      "--with-qtconf=${vlcQtEnv}/bin/qt.conf"
    ];

  postConfigure = oldAttrs.postConfigure + ''
    substituteInPlace modules/gui/qt/Makefile \
      --replace-fail \
        'MOC_CPPFLAGS = $(DEFS)' \
        'MOC_CPPFLAGS = $(DEFS) -I${kdePackages.qtdeclarative}/include'
  '';

  postFixup = builtins.replaceStrings
    [
      "/lib/vlc/vlc-cache-gen"
      "$out/vlc/plugins"
    ]
    [
      "/libexec/vlc/vlc-cache-gen"
      "$out/lib/vlc/plugins"
    ]
    oldAttrs.postFixup;

  passthru = (oldAttrs.passthru or { }) // {
    updateScript = null;
    vlcRevision = "76a78a0b3132a814d066b54c993a3b5cd6ed9243";
  };
})
