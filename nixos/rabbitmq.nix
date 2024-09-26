{ pkgs, config, makeEnable, realUsers, ... }:
makeEnable config "myModules.rabbitmq" true {
  services.rabbitmq = {
    enable = true;
  };
}
