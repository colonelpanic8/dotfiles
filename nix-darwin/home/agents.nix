{
  config,
  inputs,
  pkgs,
  ...
}: {
  imports = [
    inputs.t3code-integration.homeManagerModules.t3code-server
    ../../nix-shared/home-manager/t3code-server-config.nix
  ];

  services.t3code = {
    package = pkgs.t3code;
    repositoryRoot = config.dotfiles.checkout;
  };
}
