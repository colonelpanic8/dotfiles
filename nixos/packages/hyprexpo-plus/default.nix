{
  stdenv,
  lib,
  cmake,
  pkg-config,
  aquamarine,
  cairo,
  glslang,
  hyprland,
  hyprcursor,
  hyprgraphics,
  hyprlang,
  hyprutils,
  libdrm,
  libGL,
  libinput,
  libxcb,
  libxcb-errors,
  libxcb-wm,
  libxkbcommon,
  lua,
  pango,
  pixman,
  systemd,
  wayland,
  src,
}:
stdenv.mkDerivation {
  pname = "hyprexpo-plus";
  version = "unstable-${src.shortRev or "unknown"}";
  inherit src;

  nativeBuildInputs = [
    cmake
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
    libdrm
    libGL
    libinput
    libxcb
    libxcb-errors
    libxcb-wm
    libxkbcommon
    lua
    pango
    pixman
    systemd
    wayland
  ];

  installPhase = ''
    runHook preInstall

    install -Dm755 libhyprexpo.so "$out/lib/libhyprexpo.so"

    runHook postInstall
  '';

  meta = {
    description = "Enhanced Hyprland workspace overview plugin";
    homepage = "https://github.com/sandwichfarm/hyprexpo-plus";
    license = lib.licenses.bsd3;
    platforms = lib.platforms.linux;
  };
}
