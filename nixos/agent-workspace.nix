{
  pkgs,
  config,
  lib,
  makeEnable,
  ...
}:
# agent-sh/agent-workspace-linux: an isolated, hidden X11 desktop (Xvfb +
# openbox) that agents drive to run GUI apps / a browser without stealing focus
# on the real desktop. Driven through its complete CLI (see the
# agent-workspace-linux skill in dotfiles/agents/skills), not MCP.
makeEnable config "myModules.agentWorkspace" true {
  environment.systemPackages = lib.mkIf config.myModules.desktop.enable [pkgs.agent-workspace-linux];
}
