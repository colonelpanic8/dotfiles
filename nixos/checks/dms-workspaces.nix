{
  pkgs,
  inputs,
}:
pkgs.runCommand "dms-workspaces" {nativeBuildInputs = [pkgs.nodejs];} ''
  node ${../../dotfiles/config/dms/tests/workspaces.cjs} ${inputs.dux-shell}/quickshell/Common/WorkspaceModel.js
  touch "$out"
''
