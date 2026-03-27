{ pkgs, ... }:
let
  quillIcSrc = pkgs.fetchFromGitHub {
    owner = "dfinity";
    repo = "ic";
    rev = "2f9ae6bf5eafed03599fd29475100aca9f78ae81";
    hash = "sha256-QWJFsWZ9miWN4ql4xFXMQM1Y71nzgGCL57yAa0j7ch4=";
  };

  quillFixed = pkgs.quill.overrideAttrs (old: {
    preBuild = (old.preBuild or "") + ''
      # The vendored git dependency resolves ../ledger.did from source-git-*,
      # not from the vendor root where nixpkgs currently copies it.
      for dir in /build/quill-*-vendor/source-git-*; do
        if [ -d "$dir" ]; then
          cp ${quillIcSrc}/rs/rosetta-api/icp_ledger/ledger.did "$dir/ledger.did"
        fi
      done
    '';
  });
in
{
  environment.systemPackages = with pkgs; [
    ic-keysmith
    quillFixed
    quill-qr
  ];
}
