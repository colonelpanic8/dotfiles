{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  builtinKeyboard = pkgs.writeShellApplication {
    name = "builtin-keyboard";
    text = ''
      case "''${1:-auto}" in
        on) inhibit=0 ;;
        off) inhibit=1 ;;
        auto)
          inhibit=0
          for dev in /sys/class/input/input*; do
            if [ "$(cat "$dev/id/vendor" 2>/dev/null)" = 16c0 ] &&
              [ "$(cat "$dev/id/product" 2>/dev/null)" = 27db ]; then
              inhibit=1
              break
            fi
          done
          ;;
        *)
          echo "usage: builtin-keyboard [on|off|auto]" >&2
          exit 2
          ;;
      esac

      for dev in /sys/class/input/input*; do
        case "$(cat "$dev/name" 2>/dev/null)" in
          "AT Translated Set 2 keyboard" | "ITE Tech. Inc. ITE Device(8258) Keyboard")
            echo "$inhibit" > "$dev/inhibited"
            ;;
        esac
      done
    '';
  };
in {
  imports = [
    ../configuration.nix
  ];

  myModules.wyoming.enable = false;
  features.full.enable = true;

  nixpkgs.overlays = [
    (_final: prev: let
      stablePkgs = inputs.nixpkgs-stable.legacyPackages.${prev.stdenv.hostPlatform.system};
    in {
      shellcheck = stablePkgs.shellcheck;
      shellcheck-minimal = stablePkgs.shellcheck-minimal or stablePkgs.shellcheck;
    })
  ];

  environment.systemPackages = with pkgs; [
    android-studio
    builtinKeyboard
    perf
    zenmonitor
  ];

  # Inhibit the built-in keyboard while a MoErgo keyboard is connected.
  # keyd grabs every keyboard, so this has to happen below it, in the kernel.
  services.udev.extraRules = ''
    ACTION=="add|remove", SUBSYSTEM=="input", KERNEL=="input*", ENV{PRODUCT}=="*/16c0/27db/*|11/1/1/*|3/48d/c977/*", RUN+="${lib.getExe builtinKeyboard} auto"
  '';

  hardware.enableRedistributableFirmware = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.initrd.kernelModules = ["amdgpu"];
  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "usbhid"
    "usb_storage"
    "sd_mod"
    "rtsx_pci_sdmmc"
    "amdgpu"
  ];
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 1;
  boot.loader.efi.canTouchEfiVariables = true;

  myModules.postgres.enable = true;
  myModules.kat.enable = true;
  myModules.hyprland.gaps.enable = false;
  myModules.hyprland.cursorSize = 20;
  networking.networkmanager.enable = true;
  services.libinput.enable = true;

  services.xserver = {
    enable = true;
    videoDrivers = ["amdgpu"];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/cb96b029-df61-45d3-905b-a9435bf446df";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/1C5A-4FBB";
    fsType = "vfat";
  };

  swapDevices = [
    {
      device = "/swapfile";
      size = 16384;
    }
  ];

  networking.hostName = "jay-lenovo";
  myModules.hostIdentity = {
    emoticon = "💼";
    tmux.background = "#16a34a";
  };

  services.power-profiles-daemon.enable = false;
  services.tlp.enable = true;

  system.stateVersion = "23.05";

  home-manager.sharedModules = [
    {
      home.stateVersion = "23.05";
      gtk.font.size = lib.mkForce 9;
      gtk.gtk3.extraConfig.gtk-cursor-theme-size = lib.mkForce 20;
      dconf.settings."org/gnome/desktop/interface".text-scaling-factor = 0.9;
      dconf.settings."org/gnome/desktop/interface".cursor-size = 20;
      xdg.configFile."ghostty/config".text = lib.mkAfter ''
        font-size = 11
      '';
    }
  ];
}
