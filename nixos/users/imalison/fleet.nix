# Agent fleet credentials: agenix for imalison's Home Manager, T3 Code managed
# connections and Paseo hosts. Hosts below full still get them when they run an
# agent backend.
{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myUsers.imalison;
  t3codeEnabled = config.myModules.t3code.enable;
  runsAgentBackend = config.myModules.paseo.enable || config.myModules.t3codeServer.enable;
in {
  config = lib.mkIf (cfg.has.full || (cfg.has.dotfiles && runsAgentBackend)) {
    home-manager.users.imalison = {config, ...}: {
      imports =
        [
          inputs.agenix.homeManagerModules.default
          ../../../nix-shared/home-manager/paseo-managed-hosts.nix
        ]
        ++ lib.optionals t3codeEnabled [
          ../../../nix-shared/home-manager/t3code-managed-connections.nix
          ../../../nix-shared/home-manager/t3code-antigravity-token.nix
        ];
      age.identityPaths = ["${config.home.homeDirectory}/.ssh/id_ed25519"];
      home.packages = [
        inputs.agenix.packages."${pkgs.stdenv.hostPlatform.system}".default
      ];
    };
  };
}
