{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myModules.codexGeneratedSkills;
in {
  options.myModules.codexGeneratedSkills = {
    enable = lib.mkEnableOption "generated Codex skill setup";

    codexHome = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.codex";
      description = "Codex home directory.";
    };

    skillsDir = lib.mkOption {
      type = lib.types.str;
      default = "${cfg.codexHome}/skills";
      description = "Codex skills directory.";
    };

    primaryRuntimeDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.cache/codex-runtimes/codex-primary-runtime";
      description = "Codex primary runtime cache directory.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.activation.setupCodexGeneratedSkills = lib.hm.dag.entryAfter ["writeBoundary"] ''
      codex_home=${lib.escapeShellArg cfg.codexHome}
      skills_dir=${lib.escapeShellArg cfg.skillsDir}
      runtime_skills_root=${lib.escapeShellArg "${cfg.primaryRuntimeDir}/skills/skills"}

      mkdir -p "$skills_dir"

      if [ -x ${lib.escapeShellArg "${pkgs.codex}/bin/codex"} ]; then
        if ! CODEX_HOME="$codex_home" ${lib.escapeShellArg "${pkgs.codex}/bin/codex"} debug prompt-input "bootstrap generated Codex skills" >/dev/null; then
          echo "setupCodexGeneratedSkills: codex failed to bootstrap .system skills" >&2
        fi
      else
        echo "setupCodexGeneratedSkills: codex binary not found; skipping .system skill bootstrap" >&2
      fi

      if [ -d "$runtime_skills_root" ]; then
        tmp_dir="$(mktemp -d "''${TMPDIR:-/tmp}/codex-primary-runtime.XXXXXX")"
        trap 'rm -rf "$tmp_dir"' EXIT

        mkdir -p "$tmp_dir/codex-primary-runtime"
        for skill_name in slides spreadsheets; do
          if [ -d "$runtime_skills_root/$skill_name" ]; then
            cp -R "$runtime_skills_root/$skill_name" "$tmp_dir/codex-primary-runtime/$skill_name"
          fi
        done

        if [ "$(find "$tmp_dir/codex-primary-runtime" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ')" != "0" ]; then
          rm -rf "$skills_dir/codex-primary-runtime"
          mv "$tmp_dir/codex-primary-runtime" "$skills_dir/codex-primary-runtime"
        fi
      else
        echo "setupCodexGeneratedSkills: runtime skills not found at $runtime_skills_root; skipping codex-primary-runtime" >&2
      fi
    '';
  };
}
