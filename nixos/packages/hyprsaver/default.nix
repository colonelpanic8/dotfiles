{
  lib,
  rustPlatform,
  pkg-config,
  cmake,
  wayland,
  wayland-protocols,
  libGL,
  libxkbcommon,
  mesa,
  src,
}:
rustPlatform.buildRustPackage {
  pname = "hyprsaver";
  version = (builtins.fromTOML (builtins.readFile "${src}/Cargo.toml")).package.version;

  inherit src;

  cargoLock.lockFile = "${src}/Cargo.lock";

  nativeBuildInputs = [
    pkg-config
    cmake
  ];

  buildInputs = [
    wayland
    wayland-protocols
    libGL
    libxkbcommon
    mesa
  ];

  # The binary dlopens libwayland-client/libxkbcommon/libEGL at runtime
  # rather than linking them, so give it an rpath to find them.
  postFixup = ''
    patchelf --add-rpath ${lib.makeLibraryPath [wayland libxkbcommon libGL]} \
      $out/bin/hyprsaver
  '';

  postInstall = ''
    install -dm755 $out/share/hyprsaver/examples
    cp -r ${src}/examples/palettes $out/share/hyprsaver/examples/
    install -Dm644 ${src}/examples/hyprsaver.toml \
      $out/share/hyprsaver/examples/hyprsaver.toml
  '';

  meta = {
    description = "Wayland-native screensaver for Hyprland rendering GLSL shaders on wlr-layer-shell overlays";
    homepage = "https://github.com/maravexa/hyprsaver";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
    mainProgram = "hyprsaver";
  };
}
