{ inputs, config, pkgs, specialArgs, ... }:
specialArgs.makeEnable config "myModules.nixified-ai" false {
  imports = [
    inputs.nixified-ai.nixosModules.comfyui
  ];

  services.comfyui = {
    enable = true;
    host = "0.0.0.0";
    acceleration = "cuda";
    models = [
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
}
