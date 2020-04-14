self: super:

let
  lorriSource = (import <nixpkgs> {}).fetchurl {
    url = "https://raw.githubusercontent.com/target/lorri/master/direnv/nixos.nix";
    sha256 = "057kqbivf4xbhakz1j1b19sxd5c6p6rqhg6pwnq2zfvvmp8nmylm";
  };
  lorriBinSource = super.fetchFromGitHub {
    owner = "IvanMalison";
    repo = "lorri";
    rev = "cb966b0d4ab7f4b5861d79a19822eca6b6a50e82";
    sha256 = "1q01cjmvd1shxlwzjsi4gzdn0sx5a132bqql3xksbnhaj7ka6j3f";
  };
in
{
  lorri = (import (lorriBinSource.outPath + "/default.nix")) { pkgs = super; };
}
