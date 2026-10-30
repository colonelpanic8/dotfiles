{
  pkgs,
  inputs,
}:
pkgs.runCommand "dms-workspaces" {nativeBuildInputs = [pkgs.nodejs pkgs.patch];} ''
  cp -R ${inputs.dank-material-shell}/quickshell shell
  chmod -R u+w shell
  patch -d shell -p1 < ${../patches/dms-dotfiles-integration.patch}
  node ${../../dotfiles/config/dms/tests/workspaces.cjs} shell/Common/WorkspaceModel.js
  touch "$out"
''
