{ pkgs, config, makeEnable, realUsers, ... }:
makeEnable config "myModules.rabbitmq" false {
  services.rabbitmq = {
    enable = true;
  };
}
