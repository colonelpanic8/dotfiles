{ config, lib, pkgs, makeEnable, ... }:
makeEnable config "myModules.xremap" true {
  hardware.uinput.enable = true;
  boot.kernelModules = [ "uinput" ];

  services.udev.extraRules = lib.mkAfter ''
    KERNEL=="uinput", GROUP="input", TAG+="uaccess"
  '';

  environment.etc."xremap/config.yml".text = ''
    keymap:
      - name: Chrome emacs-ish
        application:
          only: [Google-chrome, Google-chrome-stable, google-chrome, Chromium, chromium]
        remap:
          C-a: Home
          C-e: End
          Alt-b: C-Left
          Alt-f: C-Right
  '';

  environment.systemPackages = [ pkgs.xremap ];

  systemd.user.services.xremap = {
    description = "xremap key remapper";
    wantedBy = [ "default.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.xremap}/bin/xremap /etc/xremap/config.yml --watch";
      Restart = "on-failure";
    };
  };
}
