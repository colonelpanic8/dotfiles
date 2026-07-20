{
  inputs,
  config,
  pkgs,
  specialArgs,
  ...
}: let
  system = pkgs.stdenv.hostPlatform.system;
  comfyuiOutputDirectory = "/srv/comfyui-output";

  nixifiedAiComfyuiModule = import (inputs.nixified-ai + "/flake-modules/projects/comfyui/module.nix") {
    overlays = patchedNixifiedAiOverlays;
  };

  patchedNixifiedAiOverlays = [
    patchedNixifiedAiComfyuiOverlay
    inputs.nixified-ai.overlays.models
    inputs.nixified-ai.overlays.fetchers
  ];

  patchedNixifiedAiComfyuiOverlay = final: prev: let
    upstream = inputs.nixified-ai.overlays.comfyui final prev;
  in
    (builtins.removeAttrs upstream ["python3Packages"])
    // {
      python3Packages = prev.python3Packages.overrideScope (
        python-final: python-prev: let
          extraPackages = final.lib.packagesFromDirectoryRecursive {
            inherit (python-final) callPackage;
            directory = inputs.nixified-ai + "/flake-modules/packages";
          };
          packagesAlreadyInPrev =
            builtins.filter (name: python-prev ? ${name}) (builtins.attrNames extraPackages);
          nixifiedExtraPackages = builtins.removeAttrs extraPackages packagesAlreadyInPrev;
        in
          nixifiedExtraPackages
          // {
            tokenizers = python-prev.tokenizers.overrideAttrs (old: {
              nativeBuildInputs = (old.nativeBuildInputs or []) ++ [final.clang];
              env =
                (old.env or {})
                // {
                  CC = "${final.clang}/bin/clang";
                  HOST_CC = "${final.clang}/bin/clang";
                  CC_x86_64_unknown_linux_gnu = "${final.clang}/bin/clang";
                  CFLAGS = "-O2";
                };
            });
          }
      );
    };

  qwenRapidAioNsfwV23 = pkgs.fetchurl {
    name = "Qwen-Rapid-AIO-NSFW-v23.safetensors";
    url = "https://huggingface.co/Phr00t/Qwen-Image-Edit-Rapid-AIO/resolve/main/v23/Qwen-Rapid-AIO-NSFW-v23.safetensors";
    hash = "sha256-/bkZ/IG+pj8TdZln/JLJEYFC5ccNTmeVGZIzo17vojM=";
    passthru = {
      comfyui.installPaths = ["checkpoints"];
    };
  };

  qwenImageEditPlusV2Node = pkgs.fetchurl {
    name = "nodes_qwen.py";
    url = "https://huggingface.co/Phr00t/Qwen-Image-Edit-Rapid-AIO/resolve/main/fixed-textencode-node/nodes_qwen.v2.py";
    hash = "sha256-nfliiPRmygP31/qFh61TyAIbeE9C2qvcH1mmG3HEAjg=";
  };

  qwenRapidAioWorkflow =
    pkgs.runCommand "qwen-rapid-aio-v23-nsfw-workflow.json" {
      nativeBuildInputs = [pkgs.jq];
      src = pkgs.fetchurl {
        name = "Qwen-Rapid-AIO.json";
        url = "https://huggingface.co/Phr00t/Qwen-Image-Edit-Rapid-AIO/resolve/main/Qwen-Rapid-AIO.json";
        hash = "sha256-oLAF49cJuiQFoPfH2LW2HLHoN9py2REL9i/z/q4ijec=";
      };
    } ''
      jq '
        (.nodes[] | select(.type == "CheckpointLoaderSimple") | .widgets_values[0]) = "Qwen-Rapid-AIO-NSFW-v23.safetensors"
        | (.nodes[] | select(.id == 6)) |= (
            .type = "SaveImage"
            | .title = "Save Output Image (always)"
            | .size = [320, 346]
            | .outputs = [
                {"name": "images", "type": "IMAGE", "links": null}
              ]
            | .widgets_values = ["qwen-rapid-aio-nsfw/qwen_edit"]
            | .properties = {"Node name for S&R": "SaveImage"}
          )
        | (.nodes[] | select(.id == 7) | .title) = "Input Image 1"
        | (.nodes[] | select(.id == 8) | .title) = "Optional Input Image 2"
        | (.nodes[] | select(.id == 7) | .outputs[0].links) |= ((. // []) + [19])
        | (.nodes[] | select(.id == 9) | .inputs) = [
            {"name": "width", "type": "INT", "link": 23},
            {"name": "height", "type": "INT", "link": 27}
          ]
        | (.nodes[] | select(.id == 9) | .pos) = [110, 1040]
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
                {"name": "width", "type": "INT", "links": [21]},
                {"name": "height", "type": "INT", "links": [25]},
                {"name": "batch_size", "type": "INT", "links": null}
              ],
              "properties": {
                "Node name for S&R": "GetImageSize"
              },
              "widgets_values": []
            },
            {
              "id": 11,
              "type": "PrimitiveBoolean",
              "pos": [-565, 850],
              "size": [270, 110],
              "flags": {},
              "order": 9,
              "mode": 0,
              "inputs": [
                {
                  "localized_name": "value",
                  "name": "value",
                  "type": "BOOLEAN",
                  "widget": {"name": "value"},
                  "link": null
                }
              ],
              "outputs": [
                {
                  "localized_name": "BOOLEAN",
                  "name": "BOOLEAN",
                  "type": "BOOLEAN",
                  "links": [22]
                }
              ],
              "title": "Use Original Image Width?",
              "properties": {
                "Node name for S&R": "PrimitiveBoolean",
                "cnr_id": "comfy-core",
                "ver": "0.26.1"
              },
              "widgets_values": [true]
            },
            {
              "id": 12,
              "type": "PrimitiveInt",
              "pos": [-565, 990],
              "size": [270, 110],
              "flags": {},
              "order": 10,
              "mode": 0,
              "inputs": [
                {
                  "localized_name": "value",
                  "name": "value",
                  "type": "INT",
                  "widget": {"name": "value"},
                  "link": null
                }
              ],
              "outputs": [
                {
                  "localized_name": "INT",
                  "name": "INT",
                  "type": "INT",
                  "links": [20]
                }
              ],
              "title": "Custom Width (multiple of 16)",
              "properties": {
                "Node name for S&R": "PrimitiveInt",
                "cnr_id": "comfy-core",
                "ver": "0.26.1"
              },
              "widgets_values": [768, "fixed"]
            },
            {
              "id": 13,
              "type": "PrimitiveInt",
              "pos": [-565, 1270],
              "size": [270, 110],
              "flags": {},
              "order": 11,
              "mode": 0,
              "inputs": [
                {
                  "localized_name": "value",
                  "name": "value",
                  "type": "INT",
                  "widget": {"name": "value"},
                  "link": null
                }
              ],
              "outputs": [
                {
                  "localized_name": "INT",
                  "name": "INT",
                  "type": "INT",
                  "links": [24]
                }
              ],
              "title": "Custom Height (multiple of 16)",
              "properties": {
                "Node name for S&R": "PrimitiveInt",
                "cnr_id": "comfy-core",
                "ver": "0.26.1"
              },
              "widgets_values": [768, "fixed"]
            },
            {
              "id": 14,
              "type": "ComfySwitchNode",
              "pos": [-240, 930],
              "size": [270, 130],
              "flags": {},
              "order": 12,
              "mode": 0,
              "inputs": [
                {"localized_name": "on_false", "name": "on_false", "type": "*", "link": 20},
                {"localized_name": "on_true", "name": "on_true", "type": "*", "link": 21},
                {
                  "localized_name": "switch",
                  "name": "switch",
                  "type": "BOOLEAN",
                  "widget": {"name": "switch"},
                  "link": 22
                }
              ],
              "outputs": [
                {"localized_name": "output", "name": "output", "type": "*", "links": [23]}
              ],
              "title": "Width: Custom / Original",
              "properties": {
                "Node name for S&R": "ComfySwitchNode",
                "cnr_id": "comfy-core",
                "ver": "0.26.1"
              },
              "widgets_values": [false]
            },
            {
              "id": 15,
              "type": "ComfySwitchNode",
              "pos": [-240, 1210],
              "size": [270, 130],
              "flags": {},
              "order": 14,
              "mode": 0,
              "inputs": [
                {"localized_name": "on_false", "name": "on_false", "type": "*", "link": 24},
                {"localized_name": "on_true", "name": "on_true", "type": "*", "link": 25},
                {
                  "localized_name": "switch",
                  "name": "switch",
                  "type": "BOOLEAN",
                  "widget": {"name": "switch"},
                  "link": 26
                }
              ],
              "outputs": [
                {"localized_name": "output", "name": "output", "type": "*", "links": [27]}
              ],
              "title": "Height: Custom / Original",
              "properties": {
                "Node name for S&R": "ComfySwitchNode",
                "cnr_id": "comfy-core",
                "ver": "0.26.1"
              },
              "widgets_values": [false]
            },
            {
              "id": 16,
              "type": "PrimitiveBoolean",
              "pos": [-565, 1130],
              "size": [270, 110],
              "flags": {},
              "order": 13,
              "mode": 0,
              "inputs": [
                {
                  "localized_name": "value",
                  "name": "value",
                  "type": "BOOLEAN",
                  "widget": {"name": "value"},
                  "link": null
                }
              ],
              "outputs": [
                {
                  "localized_name": "BOOLEAN",
                  "name": "BOOLEAN",
                  "type": "BOOLEAN",
                  "links": [26]
                }
              ],
              "title": "Use Original Image Height?",
              "properties": {
                "Node name for S&R": "PrimitiveBoolean",
                "cnr_id": "comfy-core",
                "ver": "0.26.1"
              },
              "widgets_values": [true]
            }
          ]
        | .links += [
            [19, 7, 0, 10, 0, "IMAGE"],
            [20, 12, 0, 14, 0, "INT"],
            [21, 10, 0, 14, 1, "INT"],
            [22, 11, 0, 14, 2, "BOOLEAN"],
            [23, 14, 0, 9, 0, "INT"],
            [24, 13, 0, 15, 0, "INT"],
            [25, 10, 1, 15, 1, "INT"],
            [26, 16, 0, 15, 2, "BOOLEAN"],
            [27, 15, 0, 9, 1, "INT"]
          ]
        | .last_node_id = 16
        | .last_link_id = 27
        | .extra.ds = {"scale": 0.82, "offset": [780, 50]}
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

  patchedComfyuiPackages =
    nixifiedComfyuiPackages
    // {
      comfyui-unwrapped = nixifiedComfyuiPackages.comfyui-unwrapped.overrideAttrs (old: {
        postPatch =
          (old.postPatch or "")
          + ''
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
        "--output-directory"
        comfyuiOutputDirectory
        "--lowvram"
        "--fp8_e4m3fn-text-enc"
        "--cache-none"
        "--reserve-vram"
        "2.0"
        "--disable-smart-memory"
      ];
      models = [
        qwenRapidAioNsfwV23
      ];
    };

    systemd.tmpfiles.rules = [
      "d ${comfyuiOutputDirectory} 2775 root users -"
    ];

    systemd.services.comfyui.serviceConfig = {
      ExecStartPre = [
        "+${pkgs.writeShellScript "install-qwen-rapid-aio-workflow" ''
          rm -f /var/lib/comfyui/workflows/Qwen-Rapid-AIO-v23-SFW.json
          rm -f /var/lib/comfyui/.local/share/comfyui/user/default/workflows/Qwen-Rapid-AIO-v23-SFW.json
          install -D -o comfyui -g comfyui -m 0644 ${qwenRapidAioWorkflow} /var/lib/comfyui/workflows/Qwen-Rapid-AIO-v23-NSFW.json
          install -D -o comfyui -g comfyui -m 0644 ${qwenRapidAioWorkflow} /var/lib/comfyui/.local/share/comfyui/user/default/workflows/Qwen-Rapid-AIO-v23-NSFW.json
          chown -R comfyui:comfyui /var/lib/comfyui/.local/share/comfyui/user/default/workflows
        ''}"
      ];
      ReadWritePaths = pkgs.lib.mkAfter [comfyuiOutputDirectory];
      SupplementaryGroups = pkgs.lib.mkAfter ["users"];
      UMask = pkgs.lib.mkForce "0002";
    };
  }
