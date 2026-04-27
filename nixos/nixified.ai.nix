{ inputs, config, pkgs, specialArgs, ... }:
let
  system = pkgs.stdenv.hostPlatform.system;

  nixifiedAiComfyuiModule =
    import (inputs.nixified-ai + "/flake-modules/projects/comfyui/module.nix") {
      overlays = patchedNixifiedAiOverlays;
    };

  patchedNixifiedAiOverlays = [
    patchedNixifiedAiComfyuiOverlay
    inputs.nixified-ai.overlays.models
    inputs.nixified-ai.overlays.fetchers
  ];

  patchedNixifiedAiComfyuiOverlay = final: prev:
    let
      upstream = inputs.nixified-ai.overlays.comfyui final prev;
    in
    (builtins.removeAttrs upstream [ "python3Packages" ]) // {
      python3Packages = prev.python3Packages.overrideScope (
        python-final: python-prev:
        let
          extraPackages = final.lib.packagesFromDirectoryRecursive {
            inherit (python-final) callPackage;
            directory = inputs.nixified-ai + "/flake-modules/packages";
          };
          packagesAlreadyInPrev =
            builtins.filter (name: python-prev ? ${name}) (builtins.attrNames extraPackages);
        in
        builtins.removeAttrs extraPackages packagesAlreadyInPrev
      );
    };

  qwenRapidAioNsfwV23 = pkgs.fetchurl {
    name = "Qwen-Rapid-AIO-NSFW-v23.safetensors";
    url = "https://huggingface.co/Phr00t/Qwen-Image-Edit-Rapid-AIO/resolve/main/v23/Qwen-Rapid-AIO-NSFW-v23.safetensors";
    hash = "sha256-/bkZ/IG+pj8TdZln/JLJEYFC5ccNTmeVGZIzo17vojM=";
    passthru = {
      comfyui.installPaths = [ "checkpoints" ];
    };
  };

  qwenImageEditPlusV2Node = pkgs.fetchurl {
    name = "nodes_qwen.py";
    url = "https://huggingface.co/Phr00t/Qwen-Image-Edit-Rapid-AIO/resolve/main/fixed-textencode-node/nodes_qwen.v2.py";
    hash = "sha256-nfliiPRmygP31/qFh61TyAIbeE9C2qvcH1mmG3HEAjg=";
  };

  qwenRapidAioWorkflow = pkgs.runCommand "qwen-rapid-aio-v23-nsfw-workflow.json" {
    nativeBuildInputs = [ pkgs.jq ];
    src = pkgs.fetchurl {
      name = "Qwen-Rapid-AIO.json";
      url = "https://huggingface.co/Phr00t/Qwen-Image-Edit-Rapid-AIO/resolve/main/Qwen-Rapid-AIO.json";
      hash = "sha256-oLAF49cJuiQFoPfH2LW2HLHoN9py2REL9i/z/q4ijec=";
    };
  } ''
    jq '
      (.nodes[] | select(.type == "CheckpointLoaderSimple") | .widgets_values[0]) = "Qwen-Rapid-AIO-NSFW-v23.safetensors"
      | (.nodes[] | select(.id == 7) | .title) = "Input Image 1"
      | (.nodes[] | select(.id == 8) | .title) = "Optional Input Image 2"
      | (.nodes[] | select(.id == 7) | .outputs[0].links) |= ((. // []) + [19])
      | (.nodes[] | select(.id == 9) | .inputs) = [
          {"name": "width", "type": "INT", "link": 20},
          {"name": "height", "type": "INT", "link": 21}
        ]
      | (.nodes[] | select(.id == 9) | .widgets_values) = [768, 768, 1]
      | .nodes += [
          {
            "id": 10,
            "type": "GetImageSize",
            "pos": [-565.0, 735.0],
            "size": [210.0, 82.0],
            "flags": {},
            "order": 8,
            "mode": 0,
            "inputs": [
              {"name": "image", "type": "IMAGE", "link": 19}
            ],
            "outputs": [
              {"name": "width", "type": "INT", "links": [20]},
              {"name": "height", "type": "INT", "links": [21]},
              {"name": "batch_size", "type": "INT", "links": null}
            ],
            "properties": {
              "Node name for S&R": "GetImageSize"
            },
            "widgets_values": []
          }
        ]
      | .links += [
          [19, 7, 0, 10, 0, "IMAGE"],
          [20, 10, 0, 9, 0, "INT"],
          [21, 10, 1, 9, 1, "INT"]
        ]
    ' "$src" > "$out"
  '';

  nixifiedComfyuiPkgs = import pkgs.path {
    inherit system;
    config = {
      allowUnfree = true;
      cudaSupport = true;
    };
    overlays = patchedNixifiedAiOverlays;
  };

  nixifiedComfyuiPackages = nixifiedComfyuiPkgs.comfyuiPackages;

  patchedComfyuiPackages = nixifiedComfyuiPackages // {
    comfyui-unwrapped = nixifiedComfyuiPackages.comfyui-unwrapped.overrideAttrs (old: {
      postPatch = (old.postPatch or "") + ''
        cp ${qwenImageEditPlusV2Node} comfy_extras/nodes_qwen.py
      '';
    });
  };

  comfyuiPackage = nixifiedComfyuiPackages.comfyui.override {
    comfyuiPackages = patchedComfyuiPackages;
  };
in
specialArgs.makeEnable config "myModules.nixified-ai" false {
  imports = [
    nixifiedAiComfyuiModule
  ];

  nixpkgs.overlays = patchedNixifiedAiOverlays;

  services.comfyui = {
    enable = true;
    package = comfyuiPackage;
    host = "0.0.0.0";
    acceleration = "cuda";
    environmentVariables = {
      PYTORCH_CUDA_ALLOC_CONF = "expandable_segments:True";
    };
    extraFlags = [
      "--lowvram"
      "--fp8_e4m3fn-text-enc"
      "--cache-none"
      "--reserve-vram"
      "2.0"
      "--disable-smart-memory"
    ];
    models = [
      qwenRapidAioNsfwV23
      (pkgs.fetchurl {
        name = "lustifySDXLNSFW_v20-inpainting.safetensors";
        url = "https://huggingface.co/andro-flock/LUSTIFY-SDXL-NSFW-checkpoint-v2-0-INPAINTING/resolve/main/lustifySDXLNSFW_v20-inpainting.safetensors";
        hash = "sha256-YV8hBx9c6PkWQNIlJTGQTOuL+HNmGVIavuSdlKX434Q=";
        passthru = {
          comfyui.installPaths = [ "checkpoints" ];
        };
      })
    ];
  };

  systemd.services.comfyui.serviceConfig.ExecStartPre = [
    "+${pkgs.writeShellScript "install-qwen-rapid-aio-workflow" ''
      rm -f /var/lib/comfyui/workflows/Qwen-Rapid-AIO-v23-SFW.json
      rm -f /var/lib/comfyui/.local/share/comfyui/user/default/workflows/Qwen-Rapid-AIO-v23-SFW.json
      install -D -m 0644 ${qwenRapidAioWorkflow} /var/lib/comfyui/workflows/Qwen-Rapid-AIO-v23-NSFW.json
      install -D -m 0644 ${qwenRapidAioWorkflow} /var/lib/comfyui/.local/share/comfyui/user/default/workflows/Qwen-Rapid-AIO-v23-NSFW.json
    ''}"
  ];
}
