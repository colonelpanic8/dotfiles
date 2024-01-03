{ pkgs, config, makeEnable, ... }:
makeEnable config "modules.postgres" false {
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_15;
    ensureDatabases = [ "railbird" "public" ];
    authentication = pkgs.lib.mkOverride 10 ''
      #type database  DBuser  CIDR-ADDRESS auth-method
      local all       all                  trust
      host  all       all  0.0.0.0/0       trust
      host  all       all  ::1/128         trust
    '';
  };
  services.pgadmin = {
    enable = true;
    initialEmail = "IvanMalison@gmail.com";
    initialPasswordFile = (builtins.toFile "password" "This is the content of the file.");
  };
}
