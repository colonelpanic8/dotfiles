{
  pkgs,
  config,
  makeEnable,
  ...
}:
makeEnable config "myModules.rabbitmq" false {
  services.rabbitmq = {
    enable = true;
  };
}
