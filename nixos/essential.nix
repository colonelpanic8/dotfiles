{
  pkgs,
  lib,
  inputs,
  ...
}: let
  git-blame-rank = inputs.git-blame-rank.packages.${pkgs.stdenv.hostPlatform.system}.default;
  keepbook = inputs.keepbook.packages.${pkgs.stdenv.hostPlatform.system}.keepbook.overrideAttrs (_: {
    # Upstream checks currently depend on TS artifacts that are not built in Nix.
    doCheck = false;
  });

  commonPkgs = lib.filter (pkg: lib.meta.availableOn pkgs.stdenv.hostPlatform pkg) ((with pkgs; [
    automake
    bazel
    bento4
    bind
    binutils
    cachix
    bubblewrap
    cmake
    direnv
    fd
    ffmpeg
    file
    gawk
    gcc
    gh
    git-fame
    git-blame-rank
    git-lfs
    git-sync
    gitFull
    gnumake
    home-manager
    htop
    ispell
    jq
    just
    keepbook
    lsof
    magic-wormhole-rs
    ncdu
    fastfetch
    neovim
    nix-index
    nix-search-cli
    pass
    patchelf
    pstree
    rclone
    ripgrep
    silver-searcher
    skim
    tmux
    unzip
    wget
    xkcdpass
    yubikey-manager
  ]) ++ lib.optionals (builtins.hasAttr "git-sync-rs" pkgs) [pkgs.git-sync-rs]);

  linuxOnly = with pkgs; [
    dex
    dpkg
    efibootmgr
    emacs-auto
    gparted
    inotify-tools
    iotop
    lshw
    mesa-demos
    pciutils
    pulseaudio
    python-with-my-packages
    runc
    sshfs
    sysz
    gdb
    udiskie
    usbutils
    tzupdate
  ];

  darwinOnly = with pkgs; [
  ];
in {
  nixpkgs.config.allowBroken = true;

  environment.systemPackages =
    commonPkgs
    ++ lib.optionals pkgs.stdenv.isLinux linuxOnly
    ++ lib.optionals pkgs.stdenv.isDarwin darwinOnly;
}
