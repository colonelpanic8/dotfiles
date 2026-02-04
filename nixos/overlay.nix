final: prev:
let
  # XXX: codex and claude-code are now provided by dedicated flakes in nix.nix:
  #   - inputs.codex-cli-nix (github:sadjow/codex-cli-nix)
  #   - inputs.claude-code-nix (github:sadjow/claude-code-nix)
  # These use cachix caches for pre-built binaries.
  # The old manual version override code is preserved below for reference.

  # # Enable/disable version overrides (set to false to use nixpkgs versions)
  # enableCodexOverride = true; # Override to get 0.93.0
  # enableClaudeCodeOverride = false; # Disabled - needs proper buildNpmPackage override

  # # Codex version override - update these values to bump the version
  # codexVersion = {
  #   version = "0.93.0";
  #   hash = "sha256-JwCwFPa4+BAMUSp567s9l2QdanL7XEhtGSR8mvlws6Q=";
  #   # Using importCargoLock requires outputHashes for git dependencies
  #   outputHashes = {
  #     "crossterm-0.28.1" = "sha256-6qCtfSMuXACKFb9ATID39XyFDIEMFDmbx6SSmNe+728=";
  #     "nucleo-0.5.0" = "sha256-Hm4SxtTSBrcWpXrtSqeO0TACbUxq3gizg1zD/6Yw/sI=";
  #     "nucleo-matcher-0.3.1" = "sha256-Hm4SxtTSBrcWpXrtSqeO0TACbUxq3gizg1zD/6Yw/sI=";
  #     "ratatui-0.29.0" = "sha256-HBvT5c8GsiCxMffNjJGLmHnvG77A6cqEL+1ARurBXho=";
  #     "runfiles-0.1.0" = "sha256-uJpVLcQh8wWZA3GPv9D8Nt43EOirajfDJ7eq/FB+tek=";
  #     "tokio-tungstenite-0.28.0" = "sha256-vJZ3S41gHtRt4UAODsjAoSCaTksgzCALiBmbWgyDCi8=";
  #     "tungstenite-0.28.0" = "sha256-CyXZp58zGlUhEor7WItjQoS499IoSP55uWqr++ia+0A=";
  #   };
  # };
  # claudeCodeVersion = {
  #   version = "2.1.22";
  #   hash = "sha256-OqvLiwB5TwZaxDvyN/+/+eueBdWNaYxd81cd5AZK/mA=";
  #   npmDepsHash = "sha256-vy7osk3UAOEgsJx9jdcGe2wICOk5Urzxh1WLAHyHM+U=";
  # };
  placeholder = null; # Dummy binding to keep let block valid
in
{
  # Fix poetry pbs-installer version constraint issue
  poetry = prev.poetry.overrideAttrs (oldAttrs: {
    dontCheckRuntimeDeps = true;
  });

  # XXX: codex and claude-code are now provided by flakes in nix.nix
  # See the overlay at the end of nixpkgs.overlays in nix.nix

  # # XXX: Don't remove this code - use enableCodexOverride flag instead.
  # # nixpkgs often lags behind and codex moves extremely quickly
  # codex = if enableCodexOverride then prev.codex.overrideAttrs (oldAttrs: rec {
  #   inherit (codexVersion) version;
  #   src = prev.fetchFromGitHub {
  #     owner = "openai";
  #     repo = "codex";
  #     tag = "rust-v${codexVersion.version}";
  #     inherit (codexVersion) hash;
  #   };
  #   # Use importCargoLock instead of fetchCargoVendor to avoid workspace parsing issues
  #   cargoDeps = prev.rustPlatform.importCargoLock {
  #     lockFile = "${src}/codex-rs/Cargo.lock";
  #     outputHashes = codexVersion.outputHashes or {};
  #   };
  #   cargoHash = null; # Not used with importCargoLock
  # }) else prev.codex;

  # # XXX: Don't remove this code - use enableClaudeCodeOverride flag instead.
  # # nixpkgs often lags behind and claude-code moves extremely quickly
  # claude-code = if enableClaudeCodeOverride then prev.claude-code.overrideAttrs (oldAttrs: {
  #   inherit (claudeCodeVersion) version npmDepsHash;
  #   src = prev.fetchurl {
  #     url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${claudeCodeVersion.version}.tgz";
  #     inherit (claudeCodeVersion) hash;
  #   };
  # }) else prev.claude-code;

  # nvidia-container-toolkit = prev.nvidia-container-toolkit.overrideAttrs(old: {
  #   postInstall = ''
  #     ${old.postInstall or ""}
  #     mv $tools/bin/nvidia-cdi-hook $tools/bin/.nvidia-cdi-hook-wrapped
  #     cat > $tools/bin/nvidia-cdi-hook <<EOF
  #     #!${final.bash}/bin/bash
  #     # Trap any errors, including crashes
  #     trap 'exit 0' ERR
  #     set +e
  #     $tools/bin/.nvidia-cdi-hook-wrapped "\$@" || true
  #     EOF
  #     chmod +x $tools/bin/nvidia-cdi-hook
  #   '';
  # });
  runc = final.stdenv.mkDerivation {
    pname = "runc-with-logging";
    version = builtins.getAttr "version" prev.runc or "unknown";

    # No sources; we're only wrapping
    src = null;
    dontUnpack = true;
    dontPatchShell = true;
    dontBuild = true;
    dontConfigure = true;

    nativeBuildInputs = [final.installShellFiles];
    buildInputs = [];
    outputs = ["out" "man"];

    installPhase = ''
      mkdir -p "$out/bin"

      cat > "$out/bin/runc" <<EOF
      #!${final.stdenv.shell}

      # If we're running as root, ensure the /var/log/debug/runc directory exists
      # with the desired permissions.
      if [ "\$(id -u)" -eq 0 ]; then
      mkdir -p /var/log/debug/runc
      chown root:users /var/log/debug/runc
      chmod 2777 /var/log/debug/runc
      fi

      # Log this invocation to /var/log/debug/runc/invocations.log
      echo "\$(date) - runc invoked with: \$@" >> /var/log/debug/runc/invocations.log

      # Hand off control to the original runc from prev.runc.
      ${prev.runc}/bin/runc --debug "\$@" > \
      >(tee -a /var/log/debug/runc/stdout.log) \
      2> >(tee -a /var/log/debug/runc/stderr.log >&2)
      EOF

      chmod +x "$out/bin/runc"

      installManPage ${prev.runc.man}/*/*.[1-9]
      mkdir -p $man
      touch $man/afile
    '';

    # Optionally inherit original metadata
    meta = prev.runc.meta // {};
  };

  synergy = prev.synergy.overrideAttrs (old: {
    patches = (old.patches or []) ++ [ ./synergy-cstdint.patch ];
  });

  rofi-systemd = prev.rofi-systemd.overrideAttrs (_: {
    src = prev.fetchFromGitHub {
      repo = "rofi-systemd";
      owner = "IvanMalison";
      rev = "078bdb833a32cc84538d329085fbfe00c7d4d1b6";
      sha256 = "sha256-ikwIc8vR2VV3bHXEtLrGgKklpz1NSRUJoJny0iRNViQ=";
    };
  });

  wyoming-satellite = prev.wyoming-satellite.overridePythonAttrs (oldAttrs: {
    src = prev.fetchFromGitHub {
      owner = "colonelpanic8";
      repo = "wyoming-satellite";
      rev = "509628a9be2cf61116b6d0475e19c0b92a855e0b";
      hash = "sha256-ewSxVv+8r2VGYNOoj8jiMogXtp1GPApcRc2BH3Q+8W8=";
    };
    build-system = with final.python3.pkgs; [ poetry-core setuptools ];
    pythonImportsCheck = [
      "wyoming_satellite"
    ];
    propagatedBuildInputs = [];
  });

  git-sync = prev.git-sync.overrideAttrs (_: {
    src = prev.fetchFromGitHub {
      repo = "git-sync";
      owner = "IvanMalison";
      rev = "92544e76553c25da2d40d06a230ecd0a6e13c114";
      sha256 = "sha256-hBtdvxAtFUMtLqGmy1wbDk796LQcYCth29fv8L0WQyQ=";
    };
  });

  # Using mainline picom with spring physics + animation fixes
  # Branch: my-picom on colonelpanic8/picom
  # Includes: spring physics curve, adaptive settling threshold, position detection fix
  picom = prev.picom.overrideAttrs (old: {
    version = "13";
    src = prev.fetchFromGitHub {
      repo = "picom";
      owner = "colonelpanic8";
      rev = "my-picom";
      sha256 = "sha256-XeciIK5q6WE9wirWEHdevfGiNzRANudFcRcRhouWTFE=";
    };
    doCheck = false;
    dontCheck = true;
  });

  expressvpn = prev.expressvpn.overrideAttrs (_: {
    src = prev.fetchurl {
      url = "https://www.expressvpn.works/clients/linux/expressvpn_3.46.0.7-1_amd64.deb";
      hash = "sha256-v0rr1s32jj79A8IGfJgSGJVlz7rSnZYn4ealOpqee0w=";
    };
  });

  gnupg_2_4_0 = prev.gnupg.overrideAttrs (_: rec {
    pname = "gnupg";
    # 2.4.1 breaks emacs
    version = "2.4.0";
    src = prev.fetchurl {
      url = "mirror://gnupg/gnupg/${pname}-${version}.tar.bz2";
      hash = "sha256-HXkVjdAdmSQx3S4/rLif2slxJ/iXhOosthDGAPsMFIM=";
    };
  });

  emacs = prev.emacs30.override {
    withNativeCompilation = true;
    withTreeSitter = true;
  };

  python-with-my-packages = let
    my-python-packages = python-packages:
    with python-packages; [
      # universal-silabs-flasher
      argcomplete
      appdirs
      ipdb
      ipython
      numpy
      openpyxl
      pip
      requests
    ];
  in
  final.python3.withPackages my-python-packages;

  pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
    (
      python-final: python-prev: {
        pysilero-vad = python-prev.pysilero-vad.overridePythonAttrs (_: {
          src = final.fetchFromGitHub {
            owner = "colonelpanic8";
            repo = "pysilero-vad";
            rev = "846caf4aa6c5f5319b87d3127dfe0aa9e88b338e";
            hash = "sha256-SjoyiHm2RiLDFbwduKsoPK4/AvQiwZ39ZsPj2etXRV0=";
          };
        });
        home-assistant-chip-wheels = python-prev.home-assistant-chip-wheels.overrideAttrs
        (oldAttrs: rec {
          bypassAttestationVerificationPatch = final.fetchpatch {
            url = "https://raw.githubusercontent.com/tronikos/chip-wheels/8a5ec21d114010723cf428ffe79e244da7562390/8766-Bypass-attestation-verification.patch";
            sha256 = "sha256-RgmlPRSfw1PPMdHBzpoK2Drrb8nEagATY8Y5ngi7x0k=";
          };
          postPatch = (oldAttrs.postPatch or "") + ''
            pushd connectedhomeip
            patch -p1 < ${bypassAttestationVerificationPatch}
            popd
          '';
        });
      }
    )
  ];
}
