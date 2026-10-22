{
  config,
  lib,
  pkgs,
  familyInputs,
  ...
}: let
  system = pkgs.stdenv.hostPlatform.system;
  upstream = familyInputs.t3code.packages.${system}.t3code;
  t3code = pkgs.symlinkJoin {
    name = "family-t3code";
    paths = [upstream];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      for executable in t3 t3code-desktop; do
        rm "$out/bin/$executable"
        makeWrapper "${upstream}/bin/$executable" "$out/bin/$executable" \
          --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [pkgs.stdenv.cc.cc.lib]}
      done
    '';
  };
in {
  options.familyInstaller.agentTools.enable = lib.mkEnableOption "T3 Code and the primary user's coding agents";
  config = lib.mkIf config.familyInstaller.agentTools.enable {
    environment.systemPackages = [
      t3code
      familyInputs.codex.packages.${system}.default
      familyInputs.claude.packages.${system}.default
    ];
  };
}
