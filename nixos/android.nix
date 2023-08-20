{ pkgs, ... }:
{
  options = {
  };
  config = {
    environment.systemPackages = with pkgs; [
      android-udev-rules
    ];
    nixpkgs.config.android_sdk.accept_license = true;
    programs.adb.enable = true;
  };
}
