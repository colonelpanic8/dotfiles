{ pkgs, ... }:
{
  services.syncthing = {
    enable = true;
    folders = {
      sync = {
        path = "/home/syncthing/sync/";
      };
    };
    settings = {
      devices = {
        ryzen-shine = { id = "IYCR3JH-BDZ4N3F-ZBUETYR-4N2UILK-FAEJJ5B-YUYMZQ3-YN63E5T-I2EHCAK"; };
        biskcomp = { id = "63SZFEC-AVA7MAL-MRJCZXN-3J4MKY6-52CK37Q-EQDN27P-7LOTYFX-UTL7YA3"; };
        nixquick = { id = "LSRG4WP-GHW7UV6-IUL5PQB-NWDWVIZ-R7PBGEH-RCHLFRR-7JBPQU2-WFEGPQP"; };
      };
    };
  };
}
