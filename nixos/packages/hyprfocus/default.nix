{
  lib,
  gcc16Stdenv,
  pkg-config,
  hyprland,
}:
gcc16Stdenv.mkDerivation {
  pname = "hyprfocus";
  version = "0.1";
  src = ./.;
  strictDeps = true;
  nativeBuildInputs = [pkg-config];
  buildInputs = [hyprland] ++ hyprland.buildInputs;
  installPhase = ''
    runHook preInstall
    install -Dm755 libhyprfocus.so "$out/lib/libhyprfocus.so"
    runHook postInstall
  '';
  meta = {
    description = "Hyprland workspace focus layout with independent window tabs";
    license = lib.licenses.bsd3;
    platforms = lib.platforms.linux;
  };
}
