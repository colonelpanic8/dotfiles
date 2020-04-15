{
  nix = {
    trustedBinaryCaches = [ https://nix.oregon.dfinity.build ];
    binaryCachePublicKeys = [
      "hydra.oregon.dfinity.build-2:KMTixHrh9DpAjF/0xU/49VEtNuGzQ71YaVIUSOLUaCM="
      "cache.dfinity.systems-1:IcOn/2SVyPGOi8i3hKhQOlyiSQotiOBKwTFmyPX5YNw="
    ];
  };
  networking.extraHosts =
  ''
    10.20.12.55 nix.oregon.dfinity.build
  '';
}
