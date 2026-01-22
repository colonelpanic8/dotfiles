final: prev:
let
  # Claude Code version override - update these values to bump the version
  claudeCodeVersion = {
    version = "2.1.14";
    hash = "sha256-fgDSKCrauOXbgPc+FmwFX3Ro8CGxwW0Jki0wZz53gpg=";
    npmDepsHash = "";
  };

  # Codex version override - update these values to bump the version
  codexVersion = {
    version = "0.86.0";
    hash = "sha256-sypqDp67nMnxSmdUs2W8TCmfe2Ye9jO3vXLOpNeqjlI=";
    cargoHash = "sha256-Ryr5mFc+StT1d+jBtRsrOzMtyEJf7W1HbMbnC84ps4s=";
  };
in
{
  # Fix poetry pbs-installer version constraint issue
  poetry = prev.poetry.overrideAttrs (oldAttrs: {
    dontCheckRuntimeDeps = true;
  });

  # Hyprland and hy3 are provided via flakes for proper plugin compatibility
  # See flake.nix inputs: hyprland and hy3
  claude-code = prev.claude-code.overrideAttrs (oldAttrs: {
    inherit (claudeCodeVersion) version npmDepsHash;
    src = prev.fetchurl {
      url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${claudeCodeVersion.version}.tgz";
      inherit (claudeCodeVersion) hash;
    };
  });

  # codex = prev.codex.overrideAttrs (oldAttrs: rec {
  #   inherit (codexVersion) version cargoHash;
  #   src = prev.fetchFromGitHub {
  #     owner = "openai";
  #     repo = "codex";
  #     tag = "rust-v${codexVersion.version}";
  #     inherit (codexVersion) hash;
  #   };
  #   cargoDeps = prev.rustPlatform.fetchCargoVendor {
  #     inherit src;
  #     sourceRoot = "${src.name}/codex-rs";
  #     hash = cargoHash;
  #   };
  # });

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

  picom = prev.picom.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      repo = "picom";
      owner = "dccsillag";
      rev = "51b21355696add83f39ccdb8dd82ff5009ba0ae5";
      sha256 = "sha256-crCwRJd859DCIC0pEerpDqdX2j8ZrNAzVaSSB3mTPN8==";
    };
    nativeBuildInputs = old.nativeBuildInputs ++ [final.pcre final.gnugrep.pcre2 final.asciidoc];
    buildInputs = old.buildInputs ++ [final.pcre];
    nativeInstallCheckInputs = [];
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
