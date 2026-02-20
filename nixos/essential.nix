{ pkgs, inputs, ... }:
let
  keepbook = inputs.keepbook.packages.${pkgs.stdenv.hostPlatform.system}.keepbook.overrideAttrs (_: {
    # Upstream checks currently depend on TS artifacts that are not built in Nix.
    doCheck = false;
  });
in
{
  nixpkgs.config.allowBroken = true;

  environment.systemPackages = with pkgs; [
    automake
    bazel
    bento4
    bind
    binutils
    cachix
    cmake
    dex
    direnv
    dpkg
    efibootmgr
    emacs-auto
    fd
    ffmpeg
    file
    gawk
    gcc
    gdb
    gh
    git-fame
    git-lfs
    git-sync
    git-sync-rs
    gitFull
    gnumake
    gparted
    home-manager
    htop
    inotify-tools
    iotop
    ispell
    jq
    just
    keepbook
    lshw
    lsof
    magic-wormhole-rs
    mesa-demos
    ncdu
    neofetch
    neovim
    nix-index
    nix-search-cli
    pass
    patchelf
    pciutils
    pstree
    pulseaudio
    python-with-my-packages
    rclone
    ripgrep
    runc
    silver-searcher
    skim
    sshfs
    sysz
    tmux
    tzupdate
    udiskie
    unzip
    usbutils
    wget
    xkcdpass
    yubikey-manager
  ];
}
