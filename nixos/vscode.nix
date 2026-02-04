{ inputs, config, makeEnable, ... }:
makeEnable config "myModules.vscode" true {
  home-manager.sharedModules = [
    inputs.vscode-server.homeModules.default
    { services.vscode-server.enable = true; }
  ];
}
