{ config, makeEnable, ... }:
makeEnable config "modules.vscode" true {
  services.vscode-server.enable = true;
}
