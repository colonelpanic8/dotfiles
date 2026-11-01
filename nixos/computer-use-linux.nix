{
  config,
  inputs,
  lib,
  ...
}: {
  # The only definition of pkgs.computer-use-linux. Hosts that skip
  # configuration.nix (rescue-usb, the standalone profiles) import this directly.
  options.myModules.computerUseLinux.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.myModules.desktop.enable or false;
    description = "Register the computer-use-linux MCP server with the agents.";
  };

  config.nixpkgs.overlays = [
    (final: _: {
      computer-use-linux = final.callPackage ./packages/computer-use-linux {
        src = inputs.computer-use-linux;
      };
    })
  ];
}
