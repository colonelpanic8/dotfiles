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

      ".codex/skills" = {
        force = true;
        source = oos "${cfg.worktreeCodexDir}/skills";
      };
    };

    home.activation.prepareCodexDirectory = lib.hm.dag.entryBefore ["checkLinkTargets"] ''
      codex_home=${lib.escapeShellArg cfg.codexHome}
      worktree_codex=${lib.escapeShellArg cfg.worktreeCodexDir}

      if [ -L "$codex_home" ]; then
        rm -f "$codex_home"
        mkdir -p "$codex_home"
      elif [ ! -e "$codex_home" ]; then
        mkdir -p "$codex_home"
      elif [ ! -d "$codex_home" ]; then
        echo "Skipping Codex setup because $codex_home is not a directory" >&2
        exit 1
      fi
    '';

    home.activation.generateCodexConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
      codex_home=${lib.escapeShellArg cfg.codexHome}
      base=${lib.escapeShellArg "${cfg.worktreeCodexDir}/config.toml"}
      local_config=${lib.escapeShellArg "${cfg.worktreeCodexDir}/config.local.toml"}
      target="$codex_home/config.toml"
      begin_marker="# BEGIN AUTO-GENERATED CODEX MACHINE STATE"
      end_marker="# END AUTO-GENERATED CODEX MACHINE STATE"

      if [ ! -r "$base" ]; then
        echo "Missing shared Codex config at $base" >&2
        exit 1
      fi

      mkdir -p "$codex_home"

      if [ -r "$target" ]; then
        mkdir -p "$(dirname "$local_config")"
        local_state="$(mktemp "$codex_home/config.local-state.XXXXXX")"
        local_tmp="$(mktemp "$codex_home/config.local.toml.XXXXXX")"
        trap 'rm -f "$tmp" "$local_state" "$local_tmp"' EXIT

        ${lib.getExe pkgs.gawk} \
          -v begin_marker="$begin_marker" \
          -v end_marker="$end_marker" '
          FNR == NR {
            if ($0 ~ /^\[[^]]+\]$/) {
              base_sections[$0] = 1
            }
            next
          }

          function flush_block() {
            if (keep && section != "" && !(section in base_sections)) {
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

        if [ -s "$local_state" ] || { [ -r "$local_config" ] && grep -Fqx "$begin_marker" "$local_config"; }; then
          if [ -r "$local_config" ]; then
            ${lib.getExe pkgs.gawk} \
              -v begin_marker="$begin_marker" \
              -v end_marker="$end_marker" '
              $0 == begin_marker {
                skip = 1
                next
              }

              $0 == end_marker {
                skip = 0
                next
              }

              !skip {
                print
              }
            ' "$local_config" > "$local_tmp"
          else
            : > "$local_tmp"
          fi

          if [ -s "$local_tmp" ] && [ -s "$local_state" ]; then
            printf '\n' >> "$local_tmp"
          fi

          if [ -s "$local_state" ]; then
            printf '%s\n' "$begin_marker" >> "$local_tmp"
            cat "$local_state" >> "$local_tmp"
            printf '%s\n' "$end_marker" >> "$local_tmp"
          fi

          chmod 600 "$local_tmp"
          mv -f "$local_tmp" "$local_config"
        else
          rm -f "$local_tmp"
        fi

        rm -f "$local_state"
      fi

      tmp="$(mktemp "$codex_home/config.toml.XXXXXX")"
      trap 'rm -f "$tmp"' EXIT
      chmod 600 "$tmp"

      cat "$base" > "$tmp"
      if [ -r "$local_config" ]; then
        printf '\n' >> "$tmp"
        cat "$local_config" >> "$tmp"
      fi

      if [ -e "$target" ] && cmp -s "$tmp" "$target"; then
        rm -f "$tmp"
      else
        mv -f "$tmp" "$target"
      fi
    '';

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
