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

  quill = with self; rustPlatform.buildRustPackage rec {
    name = "quill-${version}";
    version = "0baa53c175";

    src = fetchFromGitHub {
      owner = "dfinity";
      repo = "quill";
      rev = "6f3117c2b97195eb2b0059011063929553bc18ea";
      sha256 = "1nr31jmhx1hnqba6n5dk8qal9r843lwss6ldcdyvwz4cmybndnna";
      # date = 2021-06-18T08:16:07+02:00;
    };

    ic = fetchFromGitHub {
      owner = "dfinity";
      repo = "ic";
      rev = "779549eccfcf61ac702dfc2ee6d76ffdc2db1f7f";
      sha256 = "1r31d5hab7k1n60a7y8fw79fjgfq04cgj9krwa6r9z4isi3919v6";
    };

    registry = "file://local-registry";

    preBuild = ''
      export REGISTRY_TRANSPORT_PROTO_INCLUDES=${ic}/rs/registry/transport/proto
      export IC_BASE_TYPES_PROTO_INCLUDES=${ic}/rs/types/base_types/proto
      export IC_PROTOBUF_PROTO_INCLUDES=${ic}/rs/protobuf/def
      export IC_NNS_COMMON_PROTO_INCLUDES=${ic}/rs/nns/common/proto
      export PROTOC=${protobuf}/bin/protoc
    '';

    cargoSha256 = "0h756lkvyqwsw3984dm0ys6qrdl22isg2zh2mmzqyw8220fgdzph";

    nativeBuildInputs = [ pkg-config protobuf ];
    buildInputs = [ openssl protobuf ];
  };
}
