{
  config,
  lib,
  pkgs,
  ...
}: let
  bindings = builtins.fromJSON (builtins.readFile ../t3code-keybindings.json);
  bindingsJson = builtins.toJSON bindings;
  keybindingsPath = "${config.home.homeDirectory}/.t3/userdata/keybindings.json";
  installBindings = pkgs.writeShellScript "install-t3code-keybindings" ''
    set -eu

    keybindings_path=${lib.escapeShellArg keybindingsPath}
    if [ ! -f "$keybindings_path" ]; then
      exit 0
    fi

    keybindings_dir="$(${pkgs.coreutils}/bin/dirname "$keybindings_path")"
    temporary="$(${pkgs.coreutils}/bin/mktemp "$keybindings_dir/.keybindings.json.XXXXXX")"
    trap '${pkgs.coreutils}/bin/rm -f "$temporary"' EXIT

    ${pkgs.jq}/bin/jq \
      --argjson desired ${lib.escapeShellArg bindingsJson} \
      '($desired | map(.key)) as $keys
       | ($desired | map(.command)) as $commands
       | [.[] as $binding
          | select(($keys | index($binding.key)) == null and ($commands | index($binding.command)) == null)
          | $binding]
         + $desired' \
      "$keybindings_path" > "$temporary"

    if ${pkgs.diffutils}/bin/cmp -s "$keybindings_path" "$temporary"; then
      exit 0
    fi
    ${pkgs.coreutils}/bin/chmod 0600 "$temporary"
    ${pkgs.coreutils}/bin/mv "$temporary" "$keybindings_path"
    trap - EXIT
  '';
in {
  home.activation.installT3CodeKeybindings = lib.hm.dag.entryAfter ["writeBoundary"] ''
    ${installBindings}
  '';

  systemd.user.services.t3code-keybindings = lib.mkIf pkgs.stdenv.isLinux {
    Unit.Description = "Install personal T3 Code keybindings";
    Service = {
      Type = "oneshot";
      ExecStart = "${installBindings}";
    };
    Install.WantedBy = ["default.target"];
  };

  systemd.user.paths.t3code-keybindings = lib.mkIf pkgs.stdenv.isLinux {
    Unit.Description = "Watch the T3 Code keybindings file";
    Path = {
      PathChanged = keybindingsPath;
      Unit = "t3code-keybindings.service";
    };
    Install.WantedBy = ["default.target"];
  };
}
