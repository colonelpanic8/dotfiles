{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myModules.dank-material-shell;
  quickshellBase = inputs.quickshell-tray.packages.${pkgs.stdenv.hostPlatform.system}.default;
  quickshellUnwrapped = quickshellBase.unwrapped.overrideAttrs (old: {
    patches = (old.patches or []) ++ [./patches/quickshell-tray-watcher-address.patch];
  });
  quickshellPackage = quickshellBase.overrideAttrs (old: {
    installPhase = builtins.replaceStrings ["${quickshellBase.unwrapped}"] ["${quickshellUnwrapped}"] old.installPhase;
    passthru = old.passthru // {unwrapped = quickshellUnwrapped;};
  });
  aiUsage = pkgs.callPackage "${inputs.dank-ai-usage}/default.nix" {
    revision = inputs.dank-ai-usage.rev;
  };
  python = pkgs.python3.withPackages (p: [p.dbus-python p.pyyaml]);
  integration = pkgs.writeShellApplication {
    name = "dms-dotfiles-state";
    runtimeInputs = [python pkgs.systemd];
    text = ''
      exec python3 ${../dotfiles/config/dms/integration.py} "$@"
    '';
  };
  nvidiaSmi = pkgs.writeShellScriptBin "nvidia-smi" ''
    for status in /sys/bus/pci/drivers/nvidia/*/power/runtime_status; do
      if [ -r "$status" ]; then
        case "$(cat "$status")" in suspended|suspending) exit 1 ;; esac
      fi
    done
    if [ -r /sys/module/nvidia/version ] && \
       [ "$(cat /sys/module/nvidia/version)" != ${lib.escapeShellArg config.hardware.nvidia.package.version} ] && \
       [ -x /run/booted-system/sw/bin/nvidia-smi ]; then
      exec /run/booted-system/sw/bin/nvidia-smi "$@"
    fi
    exec ${config.hardware.nvidia.package.bin}/bin/nvidia-smi "$@"
  '';
  runtimePackages = [quickshellPackage aiUsage integration] ++ lib.optional config.myModules.nvidia.enable nvidiaSmi;
  upstreamShell = inputs.dank-material-shell.lib.mkDmsShell pkgs;
  shellPackage = pkgs.symlinkJoin {
    name = "dms-shell-dotfiles-${upstreamShell.version}";
    paths = [upstreamShell];
    nativeBuildInputs = [pkgs.patch pkgs.makeWrapper];
    postBuild = ''
      rm -rf "$out/share/quickshell/dms"
      cp -R --no-preserve=mode ${upstreamShell}/share/quickshell/dms "$out/share/quickshell/dms"
      patch -d "$out/share/quickshell/dms" -p1 < ${./patches/dms-dotfiles-integration.patch}
      install -m644 ${../dotfiles/config/dms/DotfilesService.qml} "$out/share/quickshell/dms/Services/DotfilesService.qml"
      rm "$out/bin/dms"
      makeWrapper ${upstreamShell}/bin/dms "$out/bin/dms" \
        --add-flags "-c $out/share/quickshell/dms" \
        --prefix PATH : ${lib.makeBinPath runtimePackages}
    '';
    inherit (upstreamShell) meta;
  };
in {
  options.myModules.dank-material-shell.enable =
    lib.mkEnableOption "Dank Material Shell with the personal desktop integrations"
    // {
      default = config.myModules.hyprland.enable;
    };

  config = lib.mkIf cfg.enable {
    myModules.desktop.shellUi = lib.mkDefault "dms";
    environment.systemPackages = [shellPackage quickshellPackage integration aiUsage];
    system.build.dms = shellPackage;
    system.build.quickshell-tray = quickshellPackage;

    systemd.user.services.rumno.serviceConfig.ExecCondition = "${pkgs.bash}/bin/bash ${../dotfiles/lib/bin/desktop_shell_ui} exec-condition taffybar";

    home-manager.sharedModules = [
      inputs.dank-material-shell.homeModules.dank-material-shell
      ({lib, ...}: {
        programs.dank-material-shell = {
          enable = true;
          package = shellPackage;
          quickshell.package = quickshellPackage;
          systemd.enable = true;
          enableAudioWavelength = false;
          enableCalendarEvents = false;
          plugins = {
            dankAIUsage = {
              src = "${aiUsage}/share/dms-plugins/DankAIUsage";
            };
            desktopExtras = {
              src = ../dotfiles/config/dms/plugins/DesktopExtras;
              settings = {};
            };
          };
          managePluginSettings = false;
        };

        home.activation.seedDms = lib.hm.dag.entryAfter ["linkGeneration"] ''
          mkdir -p "$HOME/.config/dms" "$HOME/.local/state/dms"
          if [ ! -e "$HOME/.config/dms/settings.json" ]; then
            install -m600 ${../dotfiles/config/dms/settings.json} "$HOME/.config/dms/settings.json"
          fi
          if [ ! -e "$HOME/.config/dms/plugin_settings.json" ]; then
            install -m600 ${../dotfiles/config/dms/plugin-settings.json} "$HOME/.config/dms/plugin_settings.json"
          fi
          if [ ! -e "$HOME/.local/state/dms/session.json" ]; then
            install -m600 ${../dotfiles/config/dms/session.json} "$HOME/.local/state/dms/session.json"
          fi
        '';

        home.packages = [integration aiUsage];
        systemd.user.services.wlsunset.Service.ExecCondition = "${pkgs.bash}/bin/bash ${../dotfiles/lib/bin/desktop_shell_ui} exec-condition taffybar";
        systemd.user.services.dms = {
          Unit.After = ["status-notifier-watcher.service"];
          Service = {
            ExecCondition = "${pkgs.bash}/bin/bash ${../dotfiles/lib/bin/desktop_shell_ui} exec-condition dms";
            Environment = ["PATH=${lib.makeBinPath runtimePackages}:/run/current-system/sw/bin:/etc/profiles/per-user/%u/bin"];
            RestartSec = 3;
          };
        };
      })
    ];
  };
}
