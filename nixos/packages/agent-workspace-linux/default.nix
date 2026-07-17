{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  makeWrapper,
  libxkbcommon,
  libx11,
  libxcb,
  xorg-server,
  xauth,
  xdpyinfo,
  xprop,
  xwininfo,
  wayland,
  vulkan-loader,
  fontconfig,
  freetype,
  libglvnd,
  openbox,
  xdotool,
  imagemagick,
  xclip,
  bubblewrap,
  chromium,
}: let
  binaries = {
    x86_64-linux = {
      triple = "x86_64-unknown-linux-gnu";
      hash = "sha256-8KADvl4V5Gb2wcWdsO17MVJLbuI83upR7EYHTSFvkVw=";
    };
    aarch64-linux = {
      triple = "aarch64-unknown-linux-gnu";
      hash = "sha256-BWqGSH8PFQ0LVwKROzxvDJvE66lEpwYDdVnRuT97DYY=";
    };
  };
  binary =
    binaries.${stdenv.hostPlatform.system}
    or (throw "agent-workspace-linux: unsupported system ${stdenv.hostPlatform.system}");
in
  stdenv.mkDerivation (finalAttrs: {
    pname = "agent-workspace-linux";
    version = "0.2.1";

    # Upstream release is a bare binary (no archive). Building from source drags
    # in Zed's GPUI stack via git dependencies, so ship the prebuilt instead.
    src = fetchurl {
      url = "https://github.com/agent-sh/agent-workspace-linux/releases/download/v${finalAttrs.version}/agent-workspace-linux-${binary.triple}";
      inherit (binary) hash;
    };

    dontUnpack = true;

    nativeBuildInputs = [autoPatchelfHook makeWrapper];

    buildInputs = [
      stdenv.cc.cc.lib
      libxkbcommon
      libxcb
    ];

    # The GPUI viewer loads its display/render stack with dlopen, so these are
    # invisible to autoPatchelf's DT_NEEDED scan and must be forced onto the
    # rpath for the `viewer` subcommand to work.
    runtimeDependencies = [
      wayland
      vulkan-loader
      fontconfig
      freetype
      libglvnd
      libx11
    ];

    installPhase = ''
      runHook preInstall
      install -Dm755 $src $out/bin/agent-workspace-linux
      runHook postInstall
    '';

    # The workspace shells out to these at runtime: Xvfb + xauth/xdpyinfo for the
    # hidden display, openbox as its WM, xdotool for input, imagemagick for
    # screenshots, xclip for the workspace clipboard, bwrap for the permission
    # ceiling, and a browser for the workspace-browser tools.
    postFixup = ''
      wrapProgram $out/bin/agent-workspace-linux \
        --prefix PATH : ${lib.makeBinPath [
        xorg-server
        xauth
        xdpyinfo
        xprop
        xwininfo
        openbox
        xdotool
        imagemagick
        xclip
        bubblewrap
        chromium
      ]}
    '';

    meta = {
      description = "Isolated agent-owned Linux desktop and browser driven over MCP";
      homepage = "https://github.com/agent-sh/agent-workspace-linux";
      license = lib.licenses.mit;
      platforms = lib.attrNames binaries;
      mainProgram = "agent-workspace-linux";
    };
  })
