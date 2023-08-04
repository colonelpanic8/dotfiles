{ pkgs, ... }:
let
  devices = {
    ryzen-shine = { id = "IYCR3JH-BDZ4N3F-ZBUETYR-4N2UILK-FAEJJ5B-YUYMZQ3-YN63E5T-I2EHCAK"; };
    biskcomp = { id = "63SZFEC-AVA7MAL-MRJCZXN-3J4MKY6-52CK37Q-EQDN27P-7LOTYFX-UTL7YA3"; };
    nixquick = { id = "6OAGJ3J-3P3R33R-ICHXDWX-IDDCUU7-ESN3Y65-2OGZMWL-R647V7N-4TA6IQM"; };
  };
  allDevices = builtins.attrNames devices;
in
{
  system.activationScripts.syncthingPermissions = {
    text = ''
      chown -R syncthing:syncthing /var/lib/syncthing
      chmod -R 2770 /var/lib/syncthing
      mkdir -p /var/lib/syncthing/sync
    '';
  };
  services.syncthing = {
    enable = true;
    settings = {
      inherit devices;
      folders = {
        sync = {
          path = "~/sync";
          devices = allDevices;
        };
      };
    };
  };
}
