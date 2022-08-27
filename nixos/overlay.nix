final: prev: {
  picom = prev.picom.overrideAttrs(_: {
    src = prev.fetchFromGitHub {
      repo = "picom";
      owner = "dccsillag";
      rev = "51b21355696add83f39ccdb8dd82ff5009ba0ae5";
      sha256 = "sha256-crCwRJd859DCIC0pEerpDqdX2j8ZrNAzVaSSB3mTPN8==";
    };
  });
}
