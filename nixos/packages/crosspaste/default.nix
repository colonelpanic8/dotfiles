{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  makeWrapper,
  alsa-lib,
  dbus,
  fontconfig,
  freetype,
  libglvnd,
  libnotify,
  libx11,
  libxcb,
  libxcrypt-legacy,
  libxext,
  libxfixes,
  libxi,
  libxinerama,
  libxkbcommon,
  libxrender,
  libxt,
  libxtst,
  systemdLibs,
  wayland,
  zlib,
}: let
  build = "2767";
  tarballs = {
    x86_64-linux = {
      arch = "amd64";
      hash = "sha256-r4b3Hu6mrAgJxeEZVugX9n5h5TZwAtC1k76wpV6JU0o=";
    };
    aarch64-linux = {
      arch = "aarch64";
      hash = "sha256-1Ups7TDCeWi+bhjPtIVpVc6miDkPRRd848j+vuQ4Nn8=";
    };
  };
  tarball =
    tarballs.${stdenv.hostPlatform.system}
    or (throw "crosspaste: unsupported system ${stdenv.hostPlatform.system}");
in
  stdenv.mkDerivation (finalAttrs: {
    pname = "crosspaste";
    version = "2.3.0";

    # Upstream's Linux release bundles its own JBR runtime and native libraries.
    src = fetchurl {
      url = "https://github.com/CrossPaste/crosspaste-desktop/releases/download/${finalAttrs.version}.${build}/crosspaste-${finalAttrs.version}-${build}-linux-${tarball.arch}.tar.gz";
      inherit (tarball) hash;
    };

    nativeBuildInputs = [autoPatchelfHook makeWrapper];

    buildInputs = [
      stdenv.cc.cc.lib
      alsa-lib
      dbus
      fontconfig
      freetype
      libglvnd
      libx11
      libxcb
      libxcrypt-legacy
      libxext
      libxi
      libxinerama
      libxkbcommon
      libxrender
      libxt
      libxtst
      systemdLibs
      wayland
      zlib
    ];

    # The clipboard monitors, notifications and Skiko load these through
    # JNA/dlopen, which autoPatchelf's DT_NEEDED scan cannot see.
    runtimeLibraryPath = lib.makeLibraryPath [
      libglvnd
      libnotify
      libx11
      libxfixes
      libxkbcommon
      libxtst
      wayland
    ];

    installPhase = ''
      runHook preInstall

      mkdir -p $out/bin $out/lib/crosspaste $out/share
      cp -r bin lib $out/lib/crosspaste/
      cp -r share/icons share/metainfo $out/share/
      install -Dm644 share/applications/com.crosspaste.desktop \
        $out/share/applications/com.crosspaste.desktop
      substituteInPlace $out/share/applications/com.crosspaste.desktop \
        --replace-fail "/usr/lib/crosspaste/bin/crosspaste" "$out/bin/crosspaste-desktop"

      runHook postInstall
    '';

    # The CLI is a GraalVM native image that segfaults once patchelf rewrites
    # its rpath, so it only gets a new interpreter.
    dontAutoPatchelf = true;

    # Wrap in place: the launcher locates lib/app relative to itself, and the
    # app writes its own path into autostart entries.
    postFixup = ''
      autoPatchelf $out/lib/crosspaste/bin $out/lib/crosspaste/lib/runtime

      cli=$out/lib/crosspaste/lib/app/bin/crosspaste-cli
      patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" $cli
      wrapProgram $cli \
        --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [libxcrypt-legacy stdenv.cc.cc.lib]}
      wrapProgram $out/lib/crosspaste/bin/crosspaste \
        --prefix LD_LIBRARY_PATH : "$runtimeLibraryPath"

      ln -s $cli $out/bin/crosspaste
      ln -s $out/lib/crosspaste/bin/crosspaste $out/bin/crosspaste-desktop
    '';

    meta = {
      description = "Universal pasteboard that syncs clipboard history across devices";
      homepage = "https://github.com/CrossPaste/crosspaste-desktop";
      license = lib.licenses.agpl3Plus;
      sourceProvenance = [lib.sourceTypes.binaryNativeCode lib.sourceTypes.binaryBytecode];
      platforms = lib.attrNames tarballs;
      mainProgram = "crosspaste";
    };
  })
