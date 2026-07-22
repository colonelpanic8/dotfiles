{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
}: let
  binaries = {
    x86_64-linux = {
      triple = "x86_64-unknown-linux-gnu";
      mainHash = "sha256-uUgAIzutlVlSw/EgZQabNFdAswEeHPoRX6lW5rUz/h8=";
      cosmicHash = "sha256-FFQPkZzZ+Sjr7gYe5tGaNjfp5VSO7qPy17UbeXJf6/A=";
    };
    aarch64-linux = {
      triple = "aarch64-unknown-linux-gnu";
      mainHash = "sha256-XyOGzEaeRbpVojnvM/lNTjQJD9Itid+xXESjpWlGLLI=";
      cosmicHash = "sha256-OJcHXy6iTO4gW6L05865WjuHskPyjrCYyMwVcquOm3Q=";
    };
  };
  binary =
    binaries.${stdenv.hostPlatform.system}
    or (throw "computer-use-linux: unsupported system ${stdenv.hostPlatform.system}");
in
  stdenv.mkDerivation (finalAttrs: {
    pname = "computer-use-linux";
    version = "0.4.1";

    src = fetchurl {
      url = "https://github.com/agent-sh/computer-use-linux/releases/download/v${finalAttrs.version}/computer-use-linux-${binary.triple}";
      hash = binary.mainHash;
    };
    cosmicSrc = fetchurl {
      url = "https://github.com/agent-sh/computer-use-linux/releases/download/v${finalAttrs.version}/computer-use-linux-cosmic-${binary.triple}";
      hash = binary.cosmicHash;
    };

    dontUnpack = true;
    strictDeps = true;
    nativeBuildInputs = [autoPatchelfHook];
    buildInputs = [stdenv.cc.cc.lib];

    installPhase = ''
      runHook preInstall
      install -Dm755 $src $out/bin/computer-use-linux
      install -Dm755 $cosmicSrc $out/bin/computer-use-linux-cosmic
      runHook postInstall
    '';

    meta = {
      description = "Linux desktop control over MCP";
      homepage = "https://github.com/agent-sh/computer-use-linux";
      license = lib.licenses.mit;
      platforms = lib.attrNames binaries;
      mainProgram = "computer-use-linux";
    };
  })
