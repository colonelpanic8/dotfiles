{ pkgs, makeEnable, config, ... }:
let
  devices = {
    biskcomp = {
      id = "PVGY6OH-WJEA5SD-SL5YI3F-72AZKVL-ZXHIWEW-D5V245P-VUM2BST-CH47JQD";
      addresses = [ "tcp://192.168.1.44:22000" "tcp://1896Folsom.duckdns.org:22000" ];
    };
    ryzen-shine = { id = "IYCR3JH-BDZ4N3F-ZBUETYR-4N2UILK-FAEJJ5B-YUYMZQ3-YN63E5T-I2EHCAK"; };
    nixquick = { id = "6OAGJ3J-3P3R33R-ICHXDWX-IDDCUU7-ESN3Y65-2OGZMWL-R647V7N-4TA6IQM"; };
    pixel-7-pro = { id = "RZGPHX3-W5BPHLT-I4VLQVI-ZW4K7CE-X525NEN-XRG6MOH-GPEUXAD-4VG3XAR"; };
    jay-lenovo-wsl = { id = "F3HW6DZ-B7BFACJ-SRAUM7Z-56TXQJ5-5WKZT5A-GKXAHVD-YLXFJ4M-G7OJQQC"; };
    jay-lenovo = { id = "AP5ZUKJ-QBVYWCN-DYHR3UB-JJXRWNI-CAORGYM-HWRKHEH-ZJSTRLN-PP5USQX"; };
  };
  allDevices = builtins.attrNames devices;
in
makeEnable config "modules.syncthing" true {
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
          copyOwnershipFromParent = true;
        };
      };
      options = {
        relaysEnabled = true;
        localAnnounceEnabled = true;
      };
    };
  };
}
