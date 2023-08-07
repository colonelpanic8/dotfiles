final: prev: {
  git-sync = prev.git-sync.overrideAttrs(_: {
    src = prev.fetchFromGitHub {
      repo = "git-sync";
      owner = "IvanMalison";
      rev = "92544e76553c25da2d40d06a230ecd0a6e13c114";
      sha256 = "sha256-hBtdvxAtFUMtLqGmy1wbDk796LQcYCth29fv8L0WQyQ=";
    };
  });

  picom = prev.picom.overrideAttrs(oldAttrs: {
    src = prev.fetchFromGitHub {
      repo = "picom";
      owner = "IvanMalison";
      rev = "267683ef7a5a63372afba98b1c2db21b825f2b18";
      sha256 = "sha256-OI1aiSf6I+KXSIEJIxE9Aj7rP0QsExJ8daitsP2YG38=";
    };
    buildInputs = [
     final.pcre2 final.xorg.xcbutil
    ] ++ final.lib.remove final.xorg.libXinerama (
      final.lib.remove final.pcre oldAttrs.buildInputs
    );
  });

  expressvpn = prev.expressvpn.overrideAttrs(_: {
    src = prev.fetchurl {
      url = "https://www.expressvpn.works/clients/linux/expressvpn_3.46.0.7-1_amd64.deb";
      hash = "sha256-v0rr1s32jj79A8IGfJgSGJVlz7rSnZYn4ealOpqee0w=";
    };
  });

  gnupg_2_4_0 = prev.gnupg.overrideAttrs(_: rec {
    pname = "gnupg";
    # 2.4.1 breaks emacs
    version = "2.4.0";
    src = prev.fetchurl {
      url = "mirror://gnupg/gnupg/${pname}-${version}.tar.bz2";
      hash = "sha256-HXkVjdAdmSQx3S4/rLif2slxJ/iXhOosthDGAPsMFIM=";
    };
  });
}
