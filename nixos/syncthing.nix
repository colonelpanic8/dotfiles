{ pkgs, ... }:
{
  services.syncthing = {
    enable = true;
    settings = {
      devices = {
        ryzen-shine = { id = "63NX4LF-DHG7DLP-P6BHDKL-QL7TFXE-3DEGJ6G-TUBNXYS-UMI35KF-BV67WQP"; };
        biskcomp = { id = "63SZFEC-AVA7MAL-MRJCZXN-3J4MKY6-52CK37Q-EQDN27P-7LOTYFX-UTL7YA3"; };
        nixquick = { id = "LSRG4WP-GHW7UV6-IUL5PQB-NWDWVIZ-R7PBGEH-RCHLFRR-7JBPQU2-WFEGPQP"; };
      };
    };
  };
}
