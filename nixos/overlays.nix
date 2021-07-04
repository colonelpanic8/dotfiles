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
  ic-keysmith = super.buildGoModule rec {
    pname = "keysmith";
    version = "1.6.0";

    src = super.fetchFromGitHub {
      owner = "dfinity";
      repo = "keysmith";
      rev = "v${version}";
      sha256 = "1z0sxirk71yabgilq8v5lz4nd2bbm1xyrd5zppif8k9jqhr6v3v3";
    };

    vendorSha256 = "1qnj1x8ydnbw5zb3hrsd1pd2lh3qbd340sbsjyrhcrksl1hdrrax";

    runVend = true;
  };
}
