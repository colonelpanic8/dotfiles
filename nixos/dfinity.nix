{
  nix = {
    trustedBinaryCaches = [ https://nix.oregon.dfinity.build ];
    binaryCachePublicKeys = [ "hydra.oregon.dfinity.build-2:KMTixHrh9DpAjF/0xU/49VEtNuGzQ71YaVIUSOLUaCM=" ];
  };

  networking.extraResolvconfConf = ''
    name_servers="10.20.13.192"
  '';
}
