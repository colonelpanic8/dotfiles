{
  config,
  makeEnable,
  ...
}:
makeEnable config "myModules.input-resilience" true {
  # This camera exposes a bogus HID button as a keyboard. Ignore only its input
  # descendants so its UVC video interfaces remain available.
  services.udev.extraRules = ''
    SUBSYSTEM=="input", ATTRS{idVendor}=="0c45", ATTRS{idProduct}=="636b", ENV{LIBINPUT_IGNORE_DEVICE}="1"
  '';

  security.polkit = {
    enable = true;
    extraConfig = ''
      polkit.addRule(function(action, subject) {
        if ((action.id == "org.freedesktop.login1.chvt" ||
             action.id == "org.freedesktop.login1.activate-session") &&
            subject.isInGroup("wheel")) {
          return polkit.Result.YES;
        }
      });
    '';
  };

  # Includes keyboard raw-mode reset, allowing SysRq+R before switching VTs.
  boot.kernel.sysctl."kernel.sysrq" = 1;
}
