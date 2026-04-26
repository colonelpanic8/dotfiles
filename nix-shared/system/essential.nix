{
  pkgs,
  lib,
  inputs,
  ...
}: let
  system = pkgs.stdenv.hostPlatform.system;
  inputPackageOrNull = inputName: packageName: let
    input = inputs.${inputName} or null;
    packages = if input == null then null else input.packages or null;
    systemPackages = if packages == null then null else packages.${system} or null;
  in
    if systemPackages == null then null else systemPackages.${packageName} or null;

  git-blame-rank = inputs.git-blame-rank.packages.${system}.default;
  coquiTtsStreamer = inputPackageOrNull "coqui-tts-streamer" "default";
  keepbook = inputs.keepbook.packages.${system}.keepbook.overrideAttrs (_: {
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
    dex
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
    git
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
    zellij
    unzip
    wget
    xkcdpass
    yubikey-manager
  ])
  ++ lib.optionals (coquiTtsStreamer != null) [coquiTtsStreamer]
  ++ lib.optionals (builtins.hasAttr "git-sync-rs" pkgs) [pkgs.git-sync-rs]);

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
    gitFull
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
