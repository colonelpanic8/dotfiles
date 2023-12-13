{ inputs, config, makeEnable, forEachUser, ... }:
makeEnable config "modules.vscode" true {
  imports = [inputs.vscode-server.homeModules.default];
  home-manager.users = forEachUser {
    services.vscode-server.enable = true;
  };
}
