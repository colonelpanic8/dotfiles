{ pkgs, inputs, ... }:
{
  security.sudo.wheelNeedsPassword = false;
  users.extraUsers = let
    extraGroups = [
      "audio"
      "adbusers"
      "disk"
      "docker"
      "networkmanager"
      "openrazer"
      "plugdev"
      "systemd-journal"
      "video"
      "wheel"
    ];
    userDefaults = {
      inherit extraGroups;
      group = "users";
      isNormalUser = true;
      createHome = true;
      shell = pkgs.zsh;
    };
  in {
    imalison = userDefaults // {
      name = "imalison";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICzGkqGJm+nrMvsrfuWOLVxXHvi0UL1ULJmyfzS9sKpy imalison@biskcomp.local"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOuO/tc728fKyctlufiehZQuKsD0XDiS/5x7TImk0Ip4 imalison@ivanm-dfinity-razer"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDt/rcYuGGlXBcRUJvzUCgOW8PNVkJJ5TwEOha1/KGM4 imalison@stevie-nixos"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIiZd2FiyTJvuvDh5hH0L3BqZV3E/kwwyau57QD7pz7C cardno:000614590850" # Dfinity Admin
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHOEt0T+Hxxat5tbkD9mSu8T271QjRrLr2EA0rIDXUNL cardno:000614590748" # Dfinity Read-Only
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDUSkj7587e+MAUNyU/KRpw9Vk++53Wv5nB+0V1QgiTO3rMQe6HJt0Tm2wi/o/T8GNjueT2D69YgkqOIF1FQwsj2EFLObcMzeBgs5gTSglqggA2I91BIc1vvgjCDpogOMAzAQGlTxRnqrEXhqG0jJtw8KIzLr9WrvWLdTT4rHtWS8RoOBgkQ8oxbggZ4vtbMBIwoIAYGRr70KBRNCsLTPLa8yEf+DDQxq1entzxSjHXHgyeBSVVpPCrBVmhjandk+lIFInjvAiAE1ZkJHSRccL73ORmgb1crwH7xlD9NwBPmypowMi8UIRMKfL2lNehT0AQIlEAikUBLMDzPIPhnwLZ imalison@ivanm-dfinity-razer.local"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMCJ08qswd3OoApAIHQwojEUJ4sre89vSngbM3x5pBP2 IvanMalison@gmail.com" # Kat's Lenovo Legion
      ];
    };
    kat = userDefaults // {
      name = "kat";
      shell = pkgs.zsh;
    };
  };

  nix.settings.trustedUsers = [ "root" "imalison" "kat" ];

  imports = [
    inputs.home-manager.nixosModule
  ];

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.imalison = import ./home-manager.nix;
}
