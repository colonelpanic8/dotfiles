{
  config,
  lib,
  pkgs,
  ...
}: let
  checkout = "/Users/Shared/dotfiles";
  # Include personal Home Manager accounts, not service accounts such as CI.
  members = lib.unique ([config.system.primaryUser] ++ builtins.attrNames config.home-manager.users);
  setup = pkgs.writeShellScript "setup-shared-dotfiles" ''
    export PATH=${lib.makeBinPath [pkgs.git]}:/usr/bin:/bin:/usr/sbin:/sbin
    ${builtins.readFile ./setup-shared-checkout.sh}
  '';
in {
  system.activationScripts.postActivation.text = lib.mkBefore ''
    ${setup} ${lib.escapeShellArgs ([checkout] ++ members)}
  '';

  # Git otherwise rejects a checkout when another group member owns it.
  home-manager.sharedModules = [
    {
      programs.git.settings.safe.directory = [checkout];
    }
  ];
}
