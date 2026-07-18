{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myModules.voxtype;
  system = pkgs.stdenv.hostPlatform.system;
  voxtypePackages = inputs.voxtype.packages.${system};
  agentVoicePreviewModel = pkgs.fetchurl {
    url = "https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-base.en.bin";
    hash = "sha256-oDd5yG3zMjB19eeWyyzlAp8A7Ihp7uP9+4l6/jbG0AI=";
  };
in {
  options.myModules.voxtype = {
    enable = lib.mkEnableOption "local push-to-talk voice dictation";

    onDemandLoading = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Load the Whisper model only when it is needed.";
    };

    gpuIsolation = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Run transcription in a subprocess so GPU memory is released after each use.";
    };

    gpuDevice = lib.mkOption {
      type = lib.types.nullOr (lib.types.ints.between 0 7);
      default = null;
      description = ''
        Vulkan GPU device index for Whisper. Set this on multi-GPU hosts so
        Voxtype does not accidentally select an integrated GPU.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.imalison = {
      imports = [inputs.voxtype.homeManagerModules.default];

      home.packages = [
        voxtypePackages.osd-gtk4
        # The voice-task launcher uses whisper-server for local, incremental
        # previews while keeping Codex authentication on the ChatGPT plan.
        pkgs.whisper-cpp-vulkan
      ];

      # Stable discovery path used by agent_voice_task for fast CPU previews.
      home.file.".local/share/agent-voice-task/whisper-preview-model.bin".source = agentVoicePreviewModel;

      programs.voxtype = {
        enable = true;
        package = voxtypePackages.vulkan;
        engine = "whisper";
        model.name = "large-v3-turbo";
        service.enable = true;

        settings = {
          hotkey.enabled = false;

          audio = {
            device = "default";
            max_duration_secs = 120;
          };

          whisper =
            {
              language = "en";
              translate = false;
              on_demand_loading = cfg.onDemandLoading;
              gpu_isolation = cfg.gpuIsolation;
              initial_prompt = "Ivan Malison, Railbird, NixOS, Hyprland, Taffybar, Emacs, Codex.";
            }
            // lib.optionalAttrs (cfg.gpuDevice != null) {
              gpu_device = cfg.gpuDevice;
            };

          output = {
            mode = "type";
            fallback_to_clipboard = true;
            pre_type_delay_ms = 100;
            notification = {
              on_recording_start = true;
              on_recording_stop = true;
              on_transcription = false;
            };
          };
        };
      };
    };
  };
}
