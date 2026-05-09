{
  pkgs,
  hyprlandConfig,
}:
pkgs.runCommand "hyprland-config-syntax" {
  nativeBuildInputs = [pkgs.lua5_4];
} ''
  cp ${hyprlandConfig} hyprland.lua
  luac -p hyprland.lua

  if grep -n 'hyprctl' hyprland.lua | grep -v 'hyprctl reload' | grep -v 'hyprctl eval' | grep -v 'hyprctl_eval' | grep -v 'hyprctl -j monitors'; then
    echo "hyprland.lua should not shell out to hyprctl for window/workspace manipulation" >&2
    exit 1
  fi

  if grep -nE 'hl[.]dsp.*[)][(][)]' hyprland.lua; then
    echo "hyprland.lua should use hl.dispatch(...) instead of calling dispatcher objects directly" >&2
    exit 1
  fi

  lua ${./smoke-test.lua} ./hyprland.lua
  touch "$out"
''
