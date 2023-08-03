{ pkgs, ... }:
let
  my-python-packages = python-packages: with python-packages; [
    appdirs
    ipdb
    ipython
    numpy
    openpyxl
    pip
    requests
    tox
    virtualenv
    virtualenvwrapper
  ];
  python-with-my-packages = pkgs.python311.withPackages my-python-packages;
in
{
  nixpkgs.config.allowBroken = true;

  environment.systemPackages = with pkgs; [
    python-with-my-packages
    (emacs29.override {
      withNativeCompilation = true;
      withTreeSitter = true;
    })

    automake
    bazel
    bind
    binutils
    cachix
    cmake
    dex
    direnv
    dpkg
    fd
    file
    gawk
    gcc
    gdb
    gitFull
    git-lfs
    git-sync
    glxinfo
    gnumake
    gparted
    htop
    inotify-tools
    iotop
    ispell
    jq
    lshw
    lsof
    magic-wormhole-rs
    ncdu
    neofetch
    nix-index
    pass
    patchelf
    pciutils
    pstree
    rclone
    rcm
    ripgrep
    silver-searcher
    tmux
    tzupdate
    udiskie
    unzip
    usbutils
    wget
    yubikey-manager
  ];
}
