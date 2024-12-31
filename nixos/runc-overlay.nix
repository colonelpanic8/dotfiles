final: prev: {
  runc = prev.runc.overrideAttrs (_: {
    src = prev.fetchFromGitHub {
      repo = "runc";
      owner = "colonelpanic8";
      rev = "4b809881415e99247d8dea2eda862491dbc9acd2";
      sha256 = "sha256-XOslGNu+ix52938SnnhmWKUaZaTQx9+/o74tnAjmo3I=";
    };
  });
}
