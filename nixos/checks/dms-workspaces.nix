{
  pkgs,
  inputs,
}:
pkgs.runCommand "dms-workspaces" {nativeBuildInputs = [pkgs.nodejs];} ''
  node ${../../dotfiles/config/dms/tests/workspaces.cjs} ${inputs.dank-material-shell}/quickshell/Common/WorkspaceModel.js
  touch "$out"
''
