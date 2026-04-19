{pkgs, ...}: {
  home.packages = with pkgs; [
    alejandra
    claude-code
    cocoapods
    codex
    nodejs
    prettier
    slack
    tea
    typescript
    vim
    yarn
  ];

  services.git-sync = {
    enable = true;
    package =
      if pkgs ? "git-sync-rs"
      then pkgs."git-sync-rs"
      else pkgs.git-sync;
    repositories = {
      org = {
        path = "/Users/kat/org";
        uri = "git@github.com:colonelpanic8/org.git";
        interval = 180;
      };
      password-store = {
        path = "/Users/kat/.password-store";
        uri = "git@github.com:IvanMalison/.password-store.git";
      };
    };
  };

  home.stateVersion = "24.05";
}
