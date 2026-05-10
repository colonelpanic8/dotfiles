{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myModules.codexGeneratedSkills;
  oos = config.lib.file.mkOutOfStoreSymlink;
in {
  options.myModules.codexGeneratedSkills = {
    enable = lib.mkEnableOption "Codex home setup";

    codexHome = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.codex";
      description = "Codex home directory.";
    };

    worktreeCodexDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/dotfiles/dotfiles/codex";
      description = "Codex dotfiles directory in the live worktree.";
    };

    localConfig = lib.mkOption {
      type = lib.types.str;
      default = "${cfg.codexHome}/config.local.toml";
      description = "Host-local Codex config fragment appended after the shared config.";
    };

    generatedStateConfig = lib.mkOption {
      type = lib.types.str;
      default = "${cfg.codexHome}/config.local-state.toml";
      description = "Codex-generated host-local state harvested from config.toml.";
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
    home.file = {
      ".codex/.gitignore" = {
        force = true;
        source = oos "${cfg.worktreeCodexDir}/.gitignore";
      };

      ".codex/AGENTS.md" = {
        force = true;
        source = oos "${cfg.worktreeCodexDir}/AGENTS.md";
      };
    };

    home.activation.prepareCodexDirectory = lib.hm.dag.entryBefore ["checkLinkTargets"] ''
      codex_home=${lib.escapeShellArg cfg.codexHome}
      skills_dir=${lib.escapeShellArg cfg.skillsDir}

      if [ -L "$codex_home" ]; then
        rm -f "$codex_home"
        mkdir -p "$codex_home"
      elif [ ! -e "$codex_home" ]; then
        mkdir -p "$codex_home"
      elif [ ! -d "$codex_home" ]; then
        echo "Skipping Codex setup because $codex_home is not a directory" >&2
        exit 1
      fi

      if [ -L "$skills_dir" ]; then
        tmp_skills="$(mktemp -d "''${TMPDIR:-/tmp}/codex-skills.XXXXXX")"
        trap 'rm -rf "$tmp_skills"' EXIT

        for generated_dir in .system codex-primary-runtime; do
          if [ -d "$skills_dir/$generated_dir" ]; then
            cp -R "$skills_dir/$generated_dir" "$tmp_skills/$generated_dir"
          fi
        done

        rm -f "$skills_dir"
        mkdir -p "$skills_dir"

        for generated_dir in "$tmp_skills"/*; do
          if [ -e "$generated_dir" ]; then
            mv "$generated_dir" "$skills_dir/"
          fi
        done
      elif [ ! -e "$skills_dir" ]; then
        mkdir -p "$skills_dir"
      elif [ ! -d "$skills_dir" ]; then
        echo "Skipping Codex skills setup because $skills_dir is not a directory" >&2
        exit 1
      fi
    '';

    home.activation.generateCodexConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
      codex_home=${lib.escapeShellArg cfg.codexHome}
      base=${lib.escapeShellArg "${cfg.worktreeCodexDir}/config.toml"}
      local_config=${lib.escapeShellArg cfg.localConfig}
      local_state_config=${lib.escapeShellArg cfg.generatedStateConfig}
      target="$codex_home/config.toml"
      begin_marker="# BEGIN AUTO-GENERATED CODEX MACHINE STATE"
      end_marker="# END AUTO-GENERATED CODEX MACHINE STATE"
      rejected_project_prefixes=${lib.escapeShellArg (
        if pkgs.stdenv.isDarwin
        then "/home/ /run/"
        else "/Users/ /Volumes/"
      )}

      if [ ! -r "$base" ]; then
        echo "Missing shared Codex config at $base" >&2
        exit 1
      fi

      mkdir -p "$codex_home"

      if [ -r "$target" ]; then
        local_state="$(mktemp "$codex_home/config.local-state.XXXXXX")"
        trap 'rm -f "$tmp" "$local_state"' EXIT

        ${lib.getExe pkgs.gawk} \
          -v begin_marker="$begin_marker" \
          -v end_marker="$end_marker" \
          -v rejected_prefixes="$rejected_project_prefixes" '
          FNR == NR {
            if ($0 ~ /^\[[^]]+\]$/) {
              base_sections[$0] = 1
            }
            next
          }

          function rejected_project(section, path, count, parts, i) {
            if (section !~ /^\[projects\."/) {
              return 0
            }

            path = section
            sub(/^\[projects\."/ , "", path)
            sub(/"\]$/, "", path)
            count = split(rejected_prefixes, parts, /[[:space:]]+/)

            for (i = 1; i <= count; i++) {
              if (parts[i] != "" && index(path, parts[i]) == 1) {
                return 1
              }
            }

            return 0
          }

          function flush_block() {
            if (keep && section != "" && !(section in base_sections) && !rejected_project(section)) {
              if (printed) {
                print ""
              }
              printf "%s", block
              printed = 1
            }
          }

          $0 == begin_marker || $0 == end_marker {
            next
          }

          /^\[[^]]+\]$/ {
            flush_block()
            section = $0
            keep = ($0 ~ /^\[projects\./ || $0 ~ /^\[tui\./)
            block = $0 "\n"
            next
          }

          section != "" {
            block = block $0 "\n"
          }

          END {
            flush_block()
          }
        ' "$base" "$target" > "$local_state"

        if [ -s "$local_state" ]; then
          chmod 600 "$local_state"
          mv -f "$local_state" "$local_state_config"
        else
          rm -f "$local_state" "$local_state_config"
        fi
      fi

      tmp="$(mktemp "$codex_home/config.toml.XXXXXX")"
      trap 'rm -f "$tmp"' EXIT
      chmod 600 "$tmp"

      cat "$base" > "$tmp"
      if [ -r "$local_config" ]; then
        printf '\n' >> "$tmp"
        cat "$local_config" >> "$tmp"
      fi

      if [ -r "$local_state_config" ]; then
        printf '\n%s\n' "$begin_marker" >> "$tmp"
        cat "$local_state_config" >> "$tmp"
        printf '%s\n' "$end_marker" >> "$tmp"
      fi

      if [ -e "$target" ] && cmp -s "$tmp" "$target"; then
        rm -f "$tmp"
      else
        mv -f "$tmp" "$target"
      fi
    '';

    home.activation.linkCodexDotfileSkills = lib.hm.dag.entryAfter ["writeBoundary"] ''
      skills_dir=${lib.escapeShellArg cfg.skillsDir}
      worktree_skills=${lib.escapeShellArg "${cfg.worktreeCodexDir}/skills"}

      if [ ! -d "$worktree_skills" ]; then
        echo "Skipping Codex dotfile skills setup because $worktree_skills is not a directory" >&2
        exit 1
      fi

      mkdir -p "$skills_dir"

      for skill in "$worktree_skills"/*; do
        [ -d "$skill" ] || continue
        [ -r "$skill/SKILL.md" ] || continue

        name="$(basename "$skill")"
        case "$name" in
          .system|codex-primary-runtime) continue ;;
        esac

        target="$skills_dir/$name"
        if [ -L "$target" ] || [ ! -e "$target" ]; then
          ln -sfn "$skill" "$target"
        elif [ ! -d "$target" ]; then
          echo "Skipping Codex skill $name because $target exists and is not a directory" >&2
        fi
      done
    '';

    home.activation.setupCodexGeneratedSkills = lib.hm.dag.entryAfter ["linkCodexDotfileSkills"] ''
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
