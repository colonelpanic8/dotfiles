{ makeEnable, config, ... }:
let
  devices = {
    adele = { id = "DAU7EFY-PDPH4H6-PIIWMPI-3CSTDZK-SZOINSZ-VCHOHGV-G54TEYD-A27S4QQ"; };
    biskcomp = {
      id = "PVGY6OH-WJEA5SD-SL5YI3F-72AZKVL-ZXHIWEW-D5V245P-VUM2BST-CH47JQD";
      addresses = [ "tcp://192.168.1.44:22000" "tcp://1896Folsom.duckdns.org:22000" ];
    };
    ryzen-shine = { id = "IYCR3JH-BDZ4N3F-ZBUETYR-4N2UILK-FAEJJ5B-YUYMZQ3-YN63E5T-I2EHCAK"; };
    nixquick = { id = "6OAGJ3J-3P3R33R-ICHXDWX-IDDCUU7-ESN3Y65-2OGZMWL-R647V7N-4TA6IQM"; };
    pixel-7-pro = { id = "RZGPHX3-W5BPHLT-I4VLQVI-ZW4K7CE-X525NEN-XRG6MOH-GPEUXAD-4VG3XAR"; };
    jay-lenovo-wsl = { id = "F3HW6DZ-B7BFACJ-SRAUM7Z-56TXQJ5-5WKZT5A-GKXAHVD-YLXFJ4M-G7OJQQC"; };
    jay-lenovo = { id = "AP5ZUKJ-QBVYWCN-DYHR3UB-JJXRWNI-CAORGYM-HWRKHEH-ZJSTRLN-PP5USQX"; };
    jimi-hendnix = { id = "55JWW5K-4NY5DKT-TR4MVPO-UNOMQKM-J7TYCRF-CCCZGPM-FVRKHID-JVFLAAV"; };
    railbird-sf = { id = "5F4FPJM-KRBFVIH-CTTB2NC-7CPVGAM-CONMH2Q-SQPRODO-CWKRFPF-HVN4AAD"; };
    dean = { id = "RIVW4FP-NZNGGGD-4ET26IC-R6CZHEU-4EBIDHX-U756VWB-W7EM3LE-3YQ6YA3"; };
    mixos = { id = "7DMMUDT-CO33EMS-LQW65MX-3BVYDBT-ZZWDJC6-SWEF6SW-ICUMWOE-ZC4IBQK"; };
    strixi-minaj-wsl = { id = "DOAEFFI-W6EZY3K-GDUHDLX-6DQPVFC-XU3G65X-5KX6RKK-EMF7ZT7-YKKKOAM"; };
    strixi-minaj = { id = "IF76WOS-WVYFAQG-ZZMIT3R-ZR6EQQH-YQP3FGZ-BJ3LJKO-56FK2XA-UREADQM"; };
    dean-zephyrus = { id = "UVIQVZQ-WFWVUQT-OTXPK7V-RXFKAVV-RY5DV2Z-XZEH3Q2-JQZIQLW-XMSB6AT"; };
  };
  allDevices = builtins.attrNames devices;
in
makeEnable config "myModules.syncthing" true {
  system.activationScripts.syncthingPermissions = {
    text = ''
      chown -R syncthing:syncthing /var/lib/syncthing
      chmod -R 2770 /var/lib/syncthing
      mkdir -p /var/lib/syncthing/sync
      mkdir -p /var/lib/syncthing/railbird
    '';
  };
  systemd.services.syncthing = {
    serviceConfig = {
      AmbientCapabilities = "CAP_CHOWN";
      CapabilityBoundingSet = "CAP_CHOWN";
    };
  };
  services.syncthing = {
    enable = true;
    settings = {
      inherit devices;
      folders = {
        sync = {
          path = "~/sync";
          devices = allDevices;
          ignorePerms = true;
          copyOwnershipFromParent = true;
        };
        railbird = {
          path = "~/railbird";
          devices = allDevices;
          ignorePerms = true;
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
