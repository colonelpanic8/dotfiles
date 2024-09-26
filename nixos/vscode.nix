{ inputs, config, makeEnable, forEachUser, ... }:
makeEnable config "myModules.vscode" true {
  home-manager.users = forEachUser {
    imports = [inputs.vscode-server.homeModules.default];
    services.vscode-server.enable = true;
  };
}
