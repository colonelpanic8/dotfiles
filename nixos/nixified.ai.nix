{ inputs, config, specialArgs, ... }:
specialArgs.makeEnable config "myModules.nixified-ai" false {
   imports = [
     inputs.nixified-ai.nixosModules.comfyui
   ];

   services.comfyui = {
     enable = true;
     host = "0.0.0.0";
   };
}
