{
  config,
  inputs,
  lib,
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

  home.activation.addT3CodeToDock = lib.hm.dag.entryAfter ["linkGeneration"] ''
    applications="$(${pkgs.coreutils}/bin/realpath "$HOME/Applications/Home Manager Apps")"
    for bundle in ${pkgs.t3code}/Applications/*.app; do
      name="''${bundle##*/}"
      app="$HOME/Applications/Home Manager Apps/$name"
      if ! ${pkgs.dockutil}/bin/dockutil --find "$app" "$HOME" >/dev/null 2>&1 \
        && ! ${pkgs.dockutil}/bin/dockutil --find "$applications/$name" "$HOME" >/dev/null 2>&1 \
        && ! ${pkgs.dockutil}/bin/dockutil --find "$bundle" "$HOME" >/dev/null 2>&1; then
        run ${pkgs.dockutil}/bin/dockutil --add "$app" --replacing "''${name%.app}" "$HOME"
      fi
    done
  '';
}
