{
  config,
  lib,
  ...
}: let
  hostIdentity = config.myModules.hostIdentity;
in {
  options = {
    myModules.hostIdentity = {
      emoticon = lib.mkOption {
        type = lib.types.str;
        default = "🖥️";
        description = "Short visual marker used when displaying this host in multiplexer status bars.";
      };

      tmux = {
        background = lib.mkOption {
          type = lib.types.str;
          default = "#222222";
          description = "tmux machine label background color for this host.";
        };

        foreground = lib.mkOption {
          type = lib.types.str;
          default = "#ffffff";
          description = "tmux status bar foreground color for this host.";
        };
      };
    };

    myModules.xmonad.picom.vSync.enable = lib.mkOption {
      default = true;
      type = lib.types.bool;
    };
  };

  config = {
    environment.etc."multiplexer-host-identity".text = ''
      MULTIPLEXER_HOST_ICON=${lib.escapeShellArg hostIdentity.emoticon}
      MULTIPLEXER_HOST_TMUX_BG=${lib.escapeShellArg hostIdentity.tmux.background}
      MULTIPLEXER_HOST_TMUX_FG=${lib.escapeShellArg hostIdentity.tmux.foreground}
    '';

    environment.etc."tmux-host-style.conf".text = ''
      set -g status-style "fg=${hostIdentity.tmux.foreground},bg=${hostIdentity.tmux.background}"
      set -g status-left-style "fg=${hostIdentity.tmux.foreground},bg=${hostIdentity.tmux.background}"
      set -g status-right-style "fg=${hostIdentity.tmux.foreground},bg=${hostIdentity.tmux.background}"
    '';
  };
}
