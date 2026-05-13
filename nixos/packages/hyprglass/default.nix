{
  stdenv,
  lib,
  gnumake,
  pkg-config,
  aquamarine,
  cairo,
  glslang,
  hyprland,
  hyprcursor,
  hyprgraphics,
  hyprlang,
  hyprutils,
  libGL,
  libxcb,
  libxcb-errors,
  libxcb-wm,
  libdrm,
  libinput,
  libxkbcommon,
  pixman,
  wayland,
  src,
}:
stdenv.mkDerivation {
  pname = "hyprglass";
  version = "unstable-${src.shortRev or "unknown"}";
  inherit src;

  nativeBuildInputs = [
    gnumake
    pkg-config
  ];

  buildInputs = [
    aquamarine
    cairo
    glslang
    hyprland
    hyprcursor
    hyprgraphics
    hyprlang
    hyprutils
    libGL
    libxcb
    libxcb-errors
    libxcb-wm
    libdrm
    libinput
    libxkbcommon
    pixman
    wayland
  ];

  installPhase = ''
    runHook preInstall

    install -Dm755 hyprglass.so "$out/lib/hyprglass.so"

    runHook postInstall
  '';

  meta = {
    description = "Liquid Glass inspired plugin for Hyprland";
    homepage = "https://github.com/hyprnux/hyprglass";
    license = lib.licenses.bsd3;
    platforms = lib.platforms.linux;
  };
}
