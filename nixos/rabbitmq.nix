{ pkgs, config, makeEnable, realUsers, ... }:
makeEnable config "modules.rabbitmq" true {
  services.rabbitmq = {
    enable = true;
  };
}
