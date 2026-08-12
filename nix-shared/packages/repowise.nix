{
  inputs,
  lib,
  pkgs,
  python313,
}: let
  workspace = inputs.uv2nix.lib.workspace.loadWorkspace {
    workspaceRoot = inputs.repowise;
  };
  overlay = workspace.mkPyprojectOverlay {
    sourcePreference = "wheel";
  };
  numpyBuildOverlay = final: prev: {
    numpy = prev.numpy.overrideAttrs (old: {
      nativeBuildInputs =
        (old.nativeBuildInputs or [])
        ++ [
          final.cython
          final.meson-python
          final.ninja
          final.packaging
          final.pyproject-metadata
        ];
    });
  };
  pythonSet =
    (pkgs.callPackage inputs.pyproject-nix.build.packages {
      python = python313;
    }).overrideScope (
      lib.composeManyExtensions [
        inputs.pyproject-build-systems.overlays.sdist
        overlay
        numpyBuildOverlay
      ]
    );
in
  pythonSet.mkVirtualEnv "repowise-env" workspace.deps.default
