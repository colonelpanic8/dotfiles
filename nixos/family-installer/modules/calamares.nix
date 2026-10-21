{
  lib,
  pkgs,
  options,
  ...
}: let
  lock = builtins.fromJSON (builtins.readFile ../flake.lock);
  targetRelease = lib.removePrefix "nixos-" lock.nodes.${lock.nodes.root.inputs.nixpkgs}.original.ref;
  prepareTarget = pkgs.writeShellApplication {
    name = "family-prepare-target";
    runtimeInputs = [pkgs.python3 pkgs.openssh pkgs.age pkgs.procps];
    text = ''
      exec python3 ${../scripts}/prepare_target.py --source ${../.} "$@"
    '';
  };
  launchInstaller = pkgs.writeShellApplication {
    name = "family-installer";
    runtimeInputs = [pkgs.xorg.xhost];
    text = ''
      if [[ -n "''${WAYLAND_DISPLAY:-}" && -n "''${DISPLAY:-}" ]]; then
        xhost +si:localuser:root >/dev/null
      fi
      exec /run/wrappers/bin/pkexec ${pkgs.calamares-nixos}/bin/calamares "$@"
    '';
  };
  installerDesktop = pkgs.makeDesktopItem {
    name = "calamares";
    desktopName = "Install System";
    exec = lib.getExe launchInstaller;
    icon = "calamares";
    categories = ["System"];
  };
  customizeExtensions = package:
    package.overrideAttrs (old: {
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.python3];
      postPatch =
        (old.postPatch or "")
        + ''
          python3 ${../scripts/patch_calamares.py} . ${lib.getExe prepareTarget} ${lib.escapeShellArg targetRelease}
          cp -r ${../calamares/family-validate} modules/family-validate
          chmod -R u+w modules/family-validate
          substituteInPlace modules/family-validate/main.py \
            --replace-fail @helper@ ${lib.getExe prepareTarget}
          cp ${../calamares/owner.conf} config/modules/family-owner.conf
          cp ${../calamares/user.svg} branding/nixos/images/family-user.svg
        '';
    });
in {
  nixpkgs.overlays = [
    (_final: prev: {
      calamares-nixos-extensions = customizeExtensions prev.calamares-nixos-extensions;
    })
  ];
  nix.settings.experimental-features = ["nix-command" "flakes"];
  programs.partition-manager.enable = true;
  security.polkit =
    {enable = true;}
    // lib.optionalAttrs (options.security.polkit ? enablePkexecWrapper) {
      enablePkexecWrapper = true;
    };
  i18n.supportedLocales = ["all"];
  environment.systemPackages = [
    (lib.hiPrio installerDesktop)
    launchInstaller
    pkgs.calamares-nixos
    pkgs.calamares-nixos-extensions
    pkgs.glibcLocales
    pkgs.git
    prepareTarget
  ];
  environment.etc."nixos-family-installer/README.md".source = ../README.md;
  environment.etc."nixos-family-installer/AGENTS.md".source = ../AGENTS.md;
  system.build.familyInstallerLauncher = launchInstaller;
  system.build.familyCalamaresExtensions = pkgs.calamares-nixos-extensions;
}
