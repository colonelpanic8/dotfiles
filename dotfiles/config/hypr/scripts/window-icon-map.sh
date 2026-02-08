#!/usr/bin/env bash
# Source this file to get icon_for_class function.
# Builds a mapping from window class → freedesktop icon name
# by scanning .desktop files for StartupWMClass and Icon fields.
#
# Usage:
#   source "$(dirname "$0")/window-icon-map.sh"
#   icon=$(icon_for_class "google-chrome")

declare -A _WINDOW_ICON_MAP

_build_window_icon_map() {
    local IFS=':'
    local -a search_dirs=()
    local dir

    for dir in ${XDG_DATA_DIRS:-/run/current-system/sw/share:/usr/share:/usr/local/share}; do
        [ -d "$dir/applications" ] && search_dirs+=("$dir/applications")
    done
    [ -d "$HOME/.local/share/applications" ] && search_dirs+=("$HOME/.local/share/applications")
    [ ${#search_dirs[@]} -eq 0 ] && return

    # Expand globs per-directory so the pattern works correctly
    local -a desktop_files=()
    for dir in "${search_dirs[@]}"; do
        desktop_files+=("$dir"/*.desktop)
    done
    [ ${#desktop_files[@]} -eq 0 ] && return

    # Single grep pass across all desktop files
    local -A file_icons file_wmclass
    local filepath line
    while IFS=: read -r filepath line; do
        case "$line" in
            Icon=*)
                [ -z "${file_icons[$filepath]:-}" ] && file_icons["$filepath"]="${line#Icon=}"
                ;;
            StartupWMClass=*)
                [ -z "${file_wmclass[$filepath]:-}" ] && file_wmclass["$filepath"]="${line#StartupWMClass=}"
                ;;
        esac
    done < <(grep -H '^Icon=\|^StartupWMClass=' "${desktop_files[@]}" 2>/dev/null)

    # Build class → icon map
    local icon wm_class bn name
    for filepath in "${!file_icons[@]}"; do
        icon="${file_icons[$filepath]}"
        [ -n "$icon" ] || continue

        wm_class="${file_wmclass[$filepath]:-}"
        if [ -n "$wm_class" ]; then
            _WINDOW_ICON_MAP["${wm_class,,}"]="$icon"
        fi

        bn="${filepath##*/}"
        name="${bn%.desktop}"
        _WINDOW_ICON_MAP["${name,,}"]="$icon"
    done
}

_build_window_icon_map

icon_for_class() {
    local class_lower="${1,,}"
    echo "${_WINDOW_ICON_MAP[$class_lower]:-$class_lower}"
}
