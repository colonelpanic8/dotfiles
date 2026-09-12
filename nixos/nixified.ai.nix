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

  pornMasterFlux2Klein9bV4TurboQ4 = pkgs.fetchurl {
    name = "PornMaster_Flux2Klein_v4_Turbo_Q4_K.gguf";
    url = "https://huggingface.co/rectangleworm/PornMaster_Klein-9b/resolve/main/unet/turbo/PornMaster_Flux2Klein_v4_Turbo_Q4_K.gguf";
    hash = "sha256-jQiw0zrRaH7H+g2dM631xc/qZWWKSZoi5Mp4sLcziXk=";
    passthru = {
      comfyui.installPaths = ["diffusion_models"];
    };
  };

  pornMasterFlux2Klein9bTextEncoderQ4 = pkgs.fetchurl {
    name = "Qwen3-VL-8B-Q4_K_S.gguf";
    url = "https://huggingface.co/rectangleworm/PornMaster_Klein-9b/resolve/main/text_encoders/Qwen3-VL-8B-Q4_K_S.gguf";
    hash = "sha256-yXe+m5+bim0CWTbo6gOW5FAFnUXgyKzjKZwQsWaKP7o=";
    passthru = {
      comfyui.installPaths = ["text_encoders"];
    };
  };

  flux2SmallVae = pkgs.fetchurl {
    name = "full_encoder_small_decoder.safetensors";
    url = "https://huggingface.co/rectangleworm/PornMaster_Klein-9b/resolve/main/vae/full_encoder_small_decoder.safetensors";
    hash = "sha256-6kJz8C0fr7+OHRws9gGO2HSGUusL808t2RFx8W8Vq2I=";
    passthru = {
      comfyui.installPaths = ["vae"];
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

  pornMasterFlux2KleinWorkflow =
    pkgs.runCommand "pornmaster-flux2-klein-9b-v4-turbo-q4-image-edit.json" {
      nativeBuildInputs = [pkgs.jq];
      src = pkgs.fetchurl {
        name = "PornMaster_F2k_9B_turbo_Single-image-editing_Automatic_V1_2026_05_27.json";
        url = "https://huggingface.co/rectangleworm/PornMaster_Klein-9b/resolve/main/example_workflows/PornMaster_F2k_9B_turbo_Single-image-editing_Automatic_V1_2026_05_27.json";
        hash = "sha256-9xtizU41xEBV4Kpkr/VoysF+yfuyqy8vCDqLiOh11sQ=";
      };
    } ''
      jq '
        (.nodes[] | select(.id == 100)) |= (
          .type = "UnetLoaderGGUF"
          | .widgets_values = ["PornMaster_Flux2Klein_v4_Turbo_Q4_K.gguf"]
          | .properties = {
              "Node name for S&R": "UnetLoaderGGUF",
              "cnr_id": "comfyui-gguf"
            }
        )
        | (.nodes[] | select(.id == 1)) |= (
            .type = "CLIPLoaderGGUF"
            | .widgets_values = ["Qwen3-VL-8B-Q4_K_S.gguf", "flux2"]
            | .properties = {
                "Node name for S&R": "CLIPLoaderGGUF",
                "cnr_id": "comfyui-gguf"
              }
          )
        | (.nodes[] | select(.id == 180) | .widgets_values) = ["full_encoder_small_decoder.safetensors"]
        | (.nodes[] | select(.id == 185) | .widgets_values) = [
            "Redraw image 1 according to the following instruction while preserving identity, age, pose, camera angle, framing, and background unless explicitly changed: describe the desired edit here. All depicted people are consenting adults."
          ]
        | (.nodes[] | select(.id == 16) | .widgets_values) = [2]
        | (.nodes[] | select(.id == 11) | .widgets_values) = ["nearest-exact", 1, 1]
        | (.nodes[] | select(.id == 201) | .widgets_values) = ["pornmaster-flux2-klein/edit"]
        | (.nodes[] | select(.id == 100) | .outputs[0].links) = [694]
        | (.nodes[] | select(.id == 179) | .outputs[0].links) = [699]
        | (.nodes[] | select(.id == 11) | .outputs[0].links) = [538, 649]
        | .nodes |= map(
            .id as $id
            | select(([237, 249, 251, 252, 255] | index($id)) | not)
          )
        | .links |= map(
            .[0] as $id
            | select(([697, 698, 701, 706, 707] | index($id)) | not)
            | if .[0] == 694 then [694, 100, 0, 16, 0, "MODEL"]
              elif .[0] == 699 then [699, 179, 0, 201, 0, "IMAGE"]
              else .
              end
          )
        | .extra.ds = {"scale": 0.72, "offset": [420, 40]}
      ' "$src" > "$out"
    '';

  pornMasterFlux2KleinDimensionsWorkflow =
    pkgs.runCommand "pornmaster-flux2-klein-9b-v4-turbo-q4-copy-or-custom-dimensions.json" {
      nativeBuildInputs = [pkgs.jq];
      src = pornMasterFlux2KleinWorkflow;
    } ''
      jq '
        (.nodes[] | select(.id == 8) | .inputs[0].link) = 720
        | (.nodes[] | select(.id == 8) | .inputs[1].link) = 721
        | (.nodes[] | select(.id == 7) | .inputs[0].link) = 722
        | (.nodes[] | select(.id == 7) | .inputs[1].link) = 723
        | (.nodes[] | select(.id == 13) | .outputs[0].links) = [710]
        | (.nodes[] | select(.id == 13) | .outputs[1].links) = [714]
        | (.nodes[] | select(.id == 179) | .outputs[0].links) = [724]
        | (.nodes[] | select(.id == 201)) |= (
            .order = 26
            | .inputs[0].link = 726
          )
        | .nodes += [
            {
              "id": 256,
              "type": "PrimitiveBoolean",
              "pos": [520, 930],
              "size": [270, 110],
              "flags": {},
              "order": 18,
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
                  "links": [711]
                }
              ],
              "title": "Use Source Width?",
              "properties": {
                "Node name for S&R": "PrimitiveBoolean",
                "cnr_id": "comfy-core"
              },
              "widgets_values": [true]
            },
            {
              "id": 257,
              "type": "PrimitiveInt",
              "pos": [520, 1060],
              "size": [270, 110],
              "flags": {},
              "order": 19,
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
                  "links": [712]
                }
              ],
              "title": "Custom Width (multiple of 16)",
              "properties": {
                "Node name for S&R": "PrimitiveInt",
                "cnr_id": "comfy-core"
              },
              "widgets_values": [1024, "fixed"]
            },
            {
              "id": 258,
              "type": "ComfySwitchNode",
              "pos": [820, 980],
              "size": [270, 130],
              "flags": {},
              "order": 20,
              "mode": 0,
              "inputs": [
                {"localized_name": "on_false", "name": "on_false", "type": "*", "link": 712},
                {"localized_name": "on_true", "name": "on_true", "type": "*", "link": 710},
                {
                  "localized_name": "switch",
                  "name": "switch",
                  "type": "BOOLEAN",
                  "widget": {"name": "switch"},
                  "link": 711
                }
              ],
              "outputs": [
                {
                  "localized_name": "output",
                  "name": "output",
                  "type": "*",
                  "links": [720, 722]
                }
              ],
              "title": "Width: Custom / Source",
              "properties": {
                "Node name for S&R": "ComfySwitchNode",
                "cnr_id": "comfy-core"
              },
              "widgets_values": [false]
            },
            {
              "id": 259,
              "type": "PrimitiveBoolean",
              "pos": [520, 1220],
              "size": [270, 110],
              "flags": {},
              "order": 21,
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
                  "links": [715]
                }
              ],
              "title": "Use Source Height?",
              "properties": {
                "Node name for S&R": "PrimitiveBoolean",
                "cnr_id": "comfy-core"
              },
              "widgets_values": [true]
            },
            {
              "id": 260,
              "type": "PrimitiveInt",
              "pos": [520, 1350],
              "size": [270, 110],
              "flags": {},
              "order": 22,
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
                  "links": [716]
                }
              ],
              "title": "Custom Height (multiple of 16)",
              "properties": {
                "Node name for S&R": "PrimitiveInt",
                "cnr_id": "comfy-core"
              },
              "widgets_values": [1024, "fixed"]
            },
            {
              "id": 261,
              "type": "ComfySwitchNode",
              "pos": [820, 1270],
              "size": [270, 130],
              "flags": {},
              "order": 23,
              "mode": 0,
              "inputs": [
                {"localized_name": "on_false", "name": "on_false", "type": "*", "link": 716},
                {"localized_name": "on_true", "name": "on_true", "type": "*", "link": 714},
                {
                  "localized_name": "switch",
                  "name": "switch",
                  "type": "BOOLEAN",
                  "widget": {"name": "switch"},
                  "link": 715
                }
              ],
              "outputs": [
                {
                  "localized_name": "output",
                  "name": "output",
                  "type": "*",
                  "links": [721, 723]
                }
              ],
              "title": "Height: Custom / Source",
              "properties": {
                "Node name for S&R": "ComfySwitchNode",
                "cnr_id": "comfy-core"
              },
              "widgets_values": [false]
            },
            {
              "id": 262,
              "type": "PrimitiveFloat",
              "pos": [1240, 820],
              "size": [300, 110],
              "flags": {},
              "order": 24,
              "mode": 0,
              "inputs": [
                {
                  "localized_name": "value",
                  "name": "value",
                  "type": "FLOAT",
                  "widget": {"name": "value"},
                  "link": null
                }
              ],
              "outputs": [
                {
                  "localized_name": "FLOAT",
                  "name": "FLOAT",
                  "type": "FLOAT",
                  "links": [725]
                }
              ],
              "title": "Final Output Scale Multiplier (1.0 = Original)",
              "properties": {
                "Node name for S&R": "PrimitiveFloat",
                "cnr_id": "comfy-core"
              },
              "widgets_values": [1.0]
            },
            {
              "id": 263,
              "type": "ImageScaleBy",
              "pos": [1570, 740],
              "size": [300, 110],
              "flags": {},
              "order": 25,
              "mode": 0,
              "inputs": [
                {"name": "image", "type": "IMAGE", "link": 724},
                {
                  "name": "upscale_method",
                  "type": "COMBO",
                  "widget": {"name": "upscale_method"},
                  "link": null
                },
                {
                  "name": "scale_by",
                  "type": "FLOAT",
                  "widget": {"name": "scale_by"},
                  "link": 725
                }
              ],
              "outputs": [
                {
                  "name": "IMAGE",
                  "type": "IMAGE",
                  "links": [726]
                }
              ],
              "title": "Final Output Resize (After Generation)",
              "properties": {
                "Node name for S&R": "ImageScaleBy",
                "cnr_id": "comfy-core"
              },
              "widgets_values": ["lanczos", 1.0]
            }
          ]
        | .links |= map(
            .[0] as $id
            | select(([689, 690, 691, 692, 699] | index($id)) | not)
          )
        | .links += [
            [710, 13, 0, 258, 1, "INT"],
            [711, 256, 0, 258, 2, "BOOLEAN"],
            [712, 257, 0, 258, 0, "INT"],
            [714, 13, 1, 261, 1, "INT"],
            [715, 259, 0, 261, 2, "BOOLEAN"],
            [716, 260, 0, 261, 0, "INT"],
            [720, 258, 0, 8, 0, "INT"],
            [721, 261, 0, 8, 1, "INT"],
            [722, 258, 0, 7, 0, "INT"],
            [723, 261, 0, 7, 1, "INT"],
            [724, 179, 0, 263, 0, "IMAGE"],
            [725, 262, 0, 263, 2, "FLOAT"],
            [726, 263, 0, 201, 0, "IMAGE"]
          ]
        | .last_node_id = 263
        | .last_link_id = 726
        | .extra.ds = {"scale": 0.68, "offset": [250, 20]}
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
      {
        disabledModules = ["services/misc/comfyui.nix"];
      }
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
        pornMasterFlux2Klein9bV4TurboQ4
        pornMasterFlux2Klein9bTextEncoderQ4
        flux2SmallVae
      ];
      customNodes = [
        nixifiedComfyuiPackages.comfyui-gguf
      ];
    };

    systemd.tmpfiles.rules = [
      "d ${comfyuiOutputDirectory} 2775 root users -"
    ];

    systemd.services.comfyui.serviceConfig = {
      ExecStartPre = [
        "+${pkgs.writeShellScript "install-comfyui-workflows" ''
          rm -f /var/lib/comfyui/workflows/Qwen-Rapid-AIO-v23-SFW.json
          rm -f /var/lib/comfyui/.local/share/comfyui/user/default/workflows/Qwen-Rapid-AIO-v23-SFW.json
          install -D -o comfyui -g comfyui -m 0644 ${qwenRapidAioWorkflow} /var/lib/comfyui/workflows/Qwen-Rapid-AIO-v23-NSFW.json
          install -D -o comfyui -g comfyui -m 0644 ${qwenRapidAioWorkflow} /var/lib/comfyui/.local/share/comfyui/user/default/workflows/Qwen-Rapid-AIO-v23-NSFW.json
          install -D -o comfyui -g comfyui -m 0644 ${pornMasterFlux2KleinWorkflow} /var/lib/comfyui/workflows/PornMaster-FLUX2-Klein-9B-v4-Turbo-Q4-Image-Edit.json
          install -D -o comfyui -g comfyui -m 0644 ${pornMasterFlux2KleinWorkflow} /var/lib/comfyui/.local/share/comfyui/user/default/workflows/PornMaster-FLUX2-Klein-9B-v4-Turbo-Q4-Image-Edit.json
          install -D -o comfyui -g comfyui -m 0644 ${pornMasterFlux2KleinDimensionsWorkflow} /var/lib/comfyui/workflows/PornMaster-FLUX2-Klein-9B-v4-Turbo-Q4-Copy-or-Custom-Dimensions.json
          install -D -o comfyui -g comfyui -m 0644 ${pornMasterFlux2KleinDimensionsWorkflow} /var/lib/comfyui/.local/share/comfyui/user/default/workflows/PornMaster-FLUX2-Klein-9B-v4-Turbo-Q4-Copy-or-Custom-Dimensions.json
          chown -R comfyui:comfyui /var/lib/comfyui/.local/share/comfyui/user/default/workflows
        ''}"
      ];
      ReadWritePaths = pkgs.lib.mkAfter [comfyuiOutputDirectory];
      SupplementaryGroups = pkgs.lib.mkAfter ["users"];
      UMask = pkgs.lib.mkForce "0002";
    };
  }
