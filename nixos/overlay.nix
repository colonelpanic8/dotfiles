final: prev:
{
  nvidia-container-toolkit = prev.nvidia-container-toolkit.overrideAttrs(old: {
    postInstall = ''
      ${old.postInstall or ""}
      mv $tools/bin/nvidia-cdi-hook $tools/bin/.nvidia-cdi-hook-wrapped
      cat > $tools/bin/nvidia-cdi-hook <<EOF
      #!${final.bash}/bin/bash
      set +e
      $tools/bin/.nvidia-cdi-hook-wrapped "\$@" || true
      EOF
      chmod +x $tools/bin/nvidia-cdi-hook
    '';
  });
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

  rofi-systemd = prev.rofi-systemd.overrideAttrs (_: {
    src = prev.fetchFromGitHub {
      repo = "rofi-systemd";
      owner = "IvanMalison";
      rev = "078bdb833a32cc84538d329085fbfe00c7d4d1b6";
      sha256 = "sha256-ikwIc8vR2VV3bHXEtLrGgKklpz1NSRUJoJny0iRNViQ=";
    };
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

  emacs = prev.emacs29.override {
    withNativeCompilation = true;
    withTreeSitter = true;
  };

  python-with-my-packages = let
    my-python-packages = python-packages:
    with python-packages; [
      argcomplete
      appdirs
      ipdb
      ipython
      numpy
      openpyxl
      pip
      requests
      tox
    ];
  in
  final.python311.withPackages my-python-packages;

  # gitea = prev.gitea.overrideAttrs(_: {
    #   src = prev.fetchFromGitHub {
      #     repo = "gitea";
      #     owner = "colonelpanic8";
      #     rev = "40e15b12bf104f8018f56e5b826d8a2f8e2587ea";
      #     sha256 = "sha256-VXP8Ga681rcKn548rOZq9I19abY0GzXRpdiYGpwyMJ4=";
      #   };
      #   go = final.buildPackages.go_1_21;
      # });
}
