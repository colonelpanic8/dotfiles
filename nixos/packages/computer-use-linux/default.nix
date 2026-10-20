{
  lib,
  rustPlatform,
  src,
  pkg-config,
  makeWrapper,
  dbus,
  fontconfig,
  libxkbcommon,
  wayland,
}: let
  manifest = builtins.fromTOML (builtins.readFile "${src}/Cargo.toml");
in
  rustPlatform.buildRustPackage {
    pname = "computer-use-linux";
    inherit (manifest.package) version;
    inherit src;

    cargoLock.lockFile = "${src}/Cargo.lock";

    nativeBuildInputs = [pkg-config makeWrapper];
    buildInputs = [dbus];

    # Desktop integration tests need a live session outside the build sandbox.
    doCheck = false;

    postInstall = ''
      for program in "$out/bin/"*; do
        wrapProgram "$program" \
          --prefix PATH : ${lib.makeBinPath [fontconfig]} \
          --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [libxkbcommon wayland]}
      done
    '';

    meta = {
      description = "Linux desktop control over MCP";
      homepage = "https://github.com/agent-sh/computer-use-linux";
      license = lib.licenses.mit;
      platforms = ["x86_64-linux" "aarch64-linux"];
      mainProgram = "computer-use-linux";
    };
  }
