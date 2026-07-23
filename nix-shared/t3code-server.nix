{
  homeDirectory,
  lib,
  localPort ? 3774,
  pkgs,
  repositoryRoot,
  tailscaleServePort ? 443,
}: let
  # T3 hydrates PATH from the login shell, but the service still needs a
  # deterministic bootstrap PATH for Tailscale and common provider tools.
  servicePath = lib.makeBinPath (with pkgs; [
    bashInteractive
    claude-code
    codex
    coreutils
    findutils
    gh
    git
    gnugrep
    gnused
    nix
    nodejs
    openssh
    ripgrep
    t3code
    tailscale
    zsh
  ]);
in
  pkgs.writeShellScript "t3code-headless-server" ''
    set -eu

    repository_root=${lib.escapeShellArg repositoryRoot}
    if [ ! -d "$repository_root" ]; then
      echo "T3 Code repository root does not exist: $repository_root" >&2
      exit 69
    fi

    export PATH=${lib.escapeShellArg "${servicePath}:/run/current-system/sw/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"}
    # Keep this independent from the Electron-owned backend and make
    # `t3 project`/`t3 auth` use the same state by default in a shell.
    export T3CODE_HOME=${lib.escapeShellArg "${homeDirectory}/.t3"}

    cd "$repository_root"
    exec ${pkgs.t3code}/bin/t3 serve \
      --host 127.0.0.1 \
      --port ${toString localPort} \
      --tailscale-serve \
      --tailscale-serve-port ${toString tailscaleServePort} \
      "$repository_root"
  ''
