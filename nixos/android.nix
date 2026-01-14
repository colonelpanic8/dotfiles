{ pkgs, ... }:
{
  options = {
  };
  config = {
    nixpkgs.config.android_sdk.accept_license = true;
    environment.systemPackages = [ pkgs.android-tools ];
  };
}
