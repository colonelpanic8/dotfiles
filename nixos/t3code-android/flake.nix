{
  # Standalone Android toolchain dev shell for building the personal patched
  # T3 Code mobile app (see ../scripts/build-t3code-apk.sh). This is a pure
  # toolchain shell (Node 24, JDK 17, Android SDK/NDK, CMake, Zig); it does not
  # depend on the T3 Code source, so the build script points `nix develop` here
  # while running the documented expo/gradle flow inside a writable copy of the
  # Nix-built patched source tree. Vendored from a local flake that upstream
  # T3 Code does not commit.
  description = "T3 Code Android build toolchain";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs @ {
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux"] (system: let
      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
          android_sdk.accept_license = true;
        };
      };

      nodejs = pkgs.nodejs_24;
      jdk = pkgs.jdk17;
      buildToolsVersion = "36.0.0";
      cmdLineToolsVersion = "8.0";
      ndkVersion = "27.1.12297006";
      expoNdkVersion = "27.0.12077973";

      androidComposition = pkgs.androidenv.composeAndroidPackages {
        inherit cmdLineToolsVersion;
        toolsVersion = "26.1.1";
        platformToolsVersion = "35.0.2";
        buildToolsVersions = [buildToolsVersion "35.0.0" "34.0.0"];
        platformVersions = ["35" "36"];
        includeSources = false;
        abiVersions = ["x86_64"];
        includeNDK = true;
        ndkVersions = [ndkVersion expoNdkVersion];
        cmakeVersions = ["3.22.1"];
        useGoogleAPIs = true;
        useGoogleTVAddOns = false;
      };

      androidSdk = androidComposition.androidsdk;
      androidHome = "${androidSdk}/libexec/android-sdk";
      aapt2Binary = "${androidHome}/build-tools/${buildToolsVersion}/aapt2";

      commonPackages = with pkgs; [
        curl
        git
        gnumake
        nodejs
        pkg-config
        python3
        watchman
        xz
      ];
    in {
      devShells = {
        default = pkgs.mkShell {
          packages = commonPackages;

          shellHook = ''
            export PATH="$PWD/node_modules/.bin:$PWD/apps/mobile/node_modules/.bin:$PATH"
            echo "T3 Code dev shell"
            echo "  node: $(node --version)"
            echo "  pnpm: $(corepack pnpm --version)"
            echo ""
            echo "Use 'nix develop .#android' for Android development."
          '';
        };

        android = pkgs.mkShell {
          packages =
            commonPackages
            ++ [jdk androidSdk pkgs.zig_0_15];

          ANDROID_HOME = androidHome;
          ANDROID_SDK_ROOT = androidHome;
          ANDROID_NDK_HOME = "${androidHome}/ndk/${ndkVersion}";
          ANDROID_NDK_ROOT = "${androidHome}/ndk/${ndkVersion}";
          GHOSTTY_ZIG = "${pkgs.zig_0_15}/bin/zig";
          JAVA_HOME = jdk.home;
          LC_ALL = "en_US.UTF-8";
          LANG = "en_US.UTF-8";
          GRADLE_OPTS = "-Dorg.gradle.project.android.aapt2FromMavenOverride=${aapt2Binary}";
          NODE_OPTIONS = "--max-old-space-size=8192";

          shellHook = ''
            export PATH="${androidHome}/platform-tools:${androidHome}/cmdline-tools/${cmdLineToolsVersion}/bin:$PWD/node_modules/.bin:$PWD/apps/mobile/node_modules/.bin:$PATH"
            echo "T3 Code Android dev shell"
            echo "  node: $(node --version)"
            echo "  pnpm: $(corepack pnpm --version)"
            echo "  java: $(java -version 2>&1 | head -n 1)"
            echo "  sdk:  $ANDROID_SDK_ROOT"
            echo "  ndk:  $ANDROID_NDK_HOME"
          '';
        };
      };
    });
}
