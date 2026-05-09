{
  pkgs,
  hyprlandConfigDir,
}:
pkgs.runCommand "hyprland-config-syntax" {
  nativeBuildInputs = [pkgs.lua5_4];
} ''
  cp -r ${hyprlandConfigDir}/. .
  chmod -R +w .
  luac -p hyprland.lua hyprland/*.lua

  if grep -Rn 'hyprctl' hyprland.lua hyprland/*.lua | grep -v 'hyprctl reload' | grep -v 'hyprctl eval' | grep -v 'hyprctl_eval' | grep -v 'hyprctl -j monitors'; then
    echo "Hyprland Lua config should not shell out to hyprctl for window/workspace manipulation" >&2
    exit 1
  fi

  if grep -RnE 'hl[.]dsp.*[)][(][)]' hyprland.lua hyprland/*.lua; then
    echo "Hyprland Lua config should use hl.dispatch(...) instead of calling dispatcher objects directly" >&2
    exit 1
  fi

  lua ${./smoke-test.lua} ./hyprland.lua
  touch "$out"
''
