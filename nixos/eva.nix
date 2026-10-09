{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: let
  evaDesktop = inputs.eva.packages.${pkgs.stdenv.hostPlatform.system}.eva-desktop;
  evaPackage =
    if config.myModules.hyprland.enable
    then
      pkgs.symlinkJoin {
        name = "${evaDesktop.name}-hyprland";
        paths = [evaDesktop];
        nativeBuildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram "$out/bin/eva-desktop" --set _JAVA_AWT_WM_NONREPARENTING 1
        '';
        meta = evaDesktop.meta;
      }
    else evaDesktop;
in {
  config = lib.mkIf (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
    environment.systemPackages = [evaPackage pkgs.computer-use-linux];

    home-manager.sharedModules = [
      ({
        lib,
        config,
        ...
      }: {
        xdg.configFile."eva/skills/computer-use-linux/SKILL.md".source = ../dotfiles/agents/skills/computer-use-linux/SKILL.md;
        home.activation.registerEvaComputerUse = lib.hm.dag.entryAfter ["writeBoundary"] ''
          evaConfigDir=${lib.escapeShellArg "${config.xdg.configHome}/eva"}
          mkdir -p "$evaConfigDir"
          chmod 700 "$evaConfigDir"
          evaConfigFile="$evaConfigDir/mcp-servers.json"
          evaConfigTemp="$(mktemp "$evaConfigDir/.mcp-servers.XXXXXX")"
          evaComputerUse=${lib.escapeShellArg (builtins.toJSON {
            command = lib.getExe pkgs.computer-use-linux;
            args = ["mcp"];
          })}
          if [ -e "$evaConfigFile" ]; then
            ${pkgs.jq}/bin/jq --argjson server "$evaComputerUse" \
              '.mcpServers = ((.mcpServers // {}) + {"computer-use-linux": $server})' \
              "$evaConfigFile" > "$evaConfigTemp"
          else
            ${pkgs.jq}/bin/jq -n --argjson server "$evaComputerUse" \
              '{mcpServers: {"computer-use-linux": $server}}' > "$evaConfigTemp"
          fi
          chmod 600 "$evaConfigTemp"
          mv -f "$evaConfigTemp" "$evaConfigFile"
        '';
        systemd.user.services.eva = {
          Unit = {
            Description = "EVA desktop assistant";
            After = ["graphical-session.target"];
            PartOf = ["graphical-session.target"];
          };
          Service = {
            ExecStart = "${lib.getExe evaPackage} tray";
            Restart = "on-abnormal";
            SuccessExitStatus = [143];
            RestartSec = 2;
          };
          Install.WantedBy = ["graphical-session.target"];
        };
      })
    ];
  };
}
