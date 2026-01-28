# Container build logic for org-agenda-api instances
# Imported by the main dotfiles flake to expose container outputs
{ pkgs, system, tangledConfig, org-agenda-api, orgApiRev, dotfilesRev }:

let
  # Build container for a specific instance
  mkInstanceContainer = instanceName: customConfigEl: overridesEl:
    let
      # Combine tangled config with instance-specific loader and overrides
      orgAgendaCustomConfig = pkgs.runCommand "org-agenda-custom-config-${instanceName}" {} ''
        mkdir -p $out

        # Copy tangled files from dotfiles
        cp ${tangledConfig}/*.el $out/ 2>/dev/null || true

        # Add instance-specific custom-config.el loader
        cp ${customConfigEl} $out/custom-config.el

        # Add optional overrides.el if provided
        ${if overridesEl != null then "cp ${overridesEl} $out/overrides.el" else ""}
      '';
    in
    org-agenda-api.lib.${system}.mkContainer {
      customElispFile = "${orgAgendaCustomConfig}/custom-config.el";
      # Use content-based tag to avoid caching issues
      tag = "${instanceName}-${orgApiRev}-${dotfilesRev}";
    };

  configsDir = ./configs;
in
{
  inherit mkInstanceContainer;

  # Pre-built instance containers
  containers = {
    colonelpanic = mkInstanceContainer "colonelpanic"
      "${configsDir}/colonelpanic/custom-config.el"
      null;
    kat = mkInstanceContainer "kat"
      "${configsDir}/kat/custom-config.el"
      null;
  };
}
