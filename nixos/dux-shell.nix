{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myModules.dux-shell;
  dmsConfigDir = "${config.dotfiles-worktree}/dotfiles/config/dms";
  # Local plugins call repo scripts (e.g. mova_agenda) like the plugins
  # themselves, out of the store so edits apply on a shell restart.
  dotfilesBin = "${config.dotfiles-worktree}/dotfiles/lib/bin";
  quickshellPackage = inputs.quickshell-tray.packages.${pkgs.stdenv.hostPlatform.system}.default;
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
  runtimePackages = [quickshellPackage aiUsage integration] ++ lib.optional (config.myModules.nvidia.enable or false) nvidiaSmi;
  forkShell = inputs.dux-shell.lib.mkDmsShell pkgs;
  shellPackage = pkgs.symlinkJoin {
    name = "dms-shell-dotfiles-${forkShell.version}";
    paths = [forkShell];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      rm "$out/bin/dms"
      makeWrapper ${forkShell}/bin/dms "$out/bin/dms" \
        --add-flags "-c ${forkShell}/share/quickshell/dms" \
        --prefix PATH : ${lib.makeBinPath runtimePackages} \
        --suffix PATH : ${dotfilesBin}
    '';
    inherit (forkShell) meta;
  };
in {
  options.myModules.dux-shell.enable =
    lib.mkEnableOption "Dux Shell with the personal desktop integrations"
    // {
      default = config.myModules.hyprland.enable;
    };

  config = lib.mkIf cfg.enable {
    myModules.desktop.shellUi = lib.mkDefault "dms";
    environment.sessionVariables.IM_HYPRLAND_SHELL_UI = lib.mkDefault config.myModules.desktop.shellUi;
    environment.systemPackages = [shellPackage quickshellPackage integration aiUsage];
    system.build.dms = shellPackage;
    system.build.quickshell-tray = quickshellPackage;
    # Without a system service the lock screen falls back to a user PAM file
    # that pins store paths.
    security.pam.services.dankshell = {};

    home-manager.sharedModules = [
      inputs.dux-shell.homeModules.dank-material-shell
      ({
        config,
        lib,
        ...
      }: {
        programs.dank-material-shell = {
          enable = true;
          package = shellPackage;
          quickshell.package = quickshellPackage;
          systemd.enable = true;
          enableAudioWavelength = false;
          enableCalendarEvents = false;
          # Local plugins link into the repo like settings.json, so QML edits
          # need only a shell restart.
          plugins = {
            dankAIUsage = {
              src = "${aiUsage}/share/dms-plugins/DankAIUsage";
            };
            desktopExtras = {
              src = config.lib.file.mkOutOfStoreSymlink "${dmsConfigDir}/plugins/DesktopExtras";
              settings = {};
            };
            desktopBrightness = {
              src = config.lib.file.mkOutOfStoreSymlink "${dmsConfigDir}/plugins/DesktopBrightness";
              settings = {};
            };
            desktopMemory = {
              src = config.lib.file.mkOutOfStoreSymlink "${dmsConfigDir}/plugins/DesktopMemory";
              settings = {};
            };
            desktopDisk = {
              src = config.lib.file.mkOutOfStoreSymlink "${dmsConfigDir}/plugins/DesktopDisk";
              settings = {};
            };
            desktopTemps = {
              src = config.lib.file.mkOutOfStoreSymlink "${dmsConfigDir}/plugins/DesktopTemps";
              settings = {};
            };
            asusProfile = {
              src = config.lib.file.mkOutOfStoreSymlink "${dmsConfigDir}/plugins/AsusProfile";
              settings = {};
            };
            orgAgenda = {
              src = config.lib.file.mkOutOfStoreSymlink "${dmsConfigDir}/plugins/OrgAgenda";
              settings = {};
            };
          };
          managePluginSettings = false;
        };

        # DMS saves through Qt's QSaveFile, which writes to a symlink's
        # target, so UI edits land in the repo.
        xdg.configFile."dms/settings.json".source = config.lib.file.mkOutOfStoreSymlink "${dmsConfigDir}/settings.json";
        xdg.configFile."dms/plugin_settings.json".source = config.lib.file.mkOutOfStoreSymlink "${dmsConfigDir}/plugin_settings.json";

        home.activation.seedDms = lib.hm.dag.entryAfter ["linkGeneration"] ''
          mkdir -p "$HOME/.local/state/dms"
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
