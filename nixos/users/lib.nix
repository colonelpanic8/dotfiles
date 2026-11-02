lib: let
  levels = ["access" "dotfiles" "full"];
  rank = level: lib.lists.findFirstIndex (l: l == level) null levels;
in {
  # Each user directory declares `options.myUsers.<name> = mkUser "<name>" [...]`;
  # extra modules add that user's own settings to the submodule.
  mkUser = name: extraModules:
    lib.mkOption {
      default = {};
      description = "How the ${name} account is set up on this host.";
      type = lib.types.submoduleWith {
        modules =
          [
            ({config, ...}: {
              options = {
                enable = lib.mkEnableOption "the ${name} account";
                level = lib.mkOption {
                  type = lib.types.enum levels;
                  default = "dotfiles";
                  description = ''
                    access: the account, its authorized keys and groups, nothing in its home.
                    dotfiles: plus its Home Manager configuration.
                    full: plus secrets, private repositories and personal services.
                  '';
                };
                has = lib.mkOption {
                  type = lib.types.attrsOf lib.types.bool;
                  readOnly = true;
                  default = lib.genAttrs levels (level: config.enable && rank config.level >= rank level);
                  description = "Whether the account is enabled at or above each level.";
                };
              };
            })
          ]
          ++ extraModules;
      };
    };
}
