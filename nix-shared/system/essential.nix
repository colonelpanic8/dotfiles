{
  pkgs,
  lib,
  inputs,
  config ? {},
  ...
}: let
  system = pkgs.stdenv.hostPlatform.system;
  isJayLenovo = (config.networking.hostName or null) == "jay-lenovo";
  inputPackageOrNull = inputName: packageName: let
    input = inputs.${inputName} or null;
    packages =
      if input == null
      then null
      else input.packages or null;
    systemPackages =
      if packages == null
      then null
      else packages.${system} or null;
  in
    if systemPackages == null
    then null
    else systemPackages.${packageName} or null;

  git-blame-rank = inputs.git-blame-rank.packages.${system}.default.overrideAttrs (old: {
    env =
      (old.env or {})
      // {
        CARGO_BUILD_JOBS = "1";
      };
  });
  ccusage-fleet = pkgs.callPackage ../packages/ccusage-fleet.nix {
    src = inputs.ccusage-fleet;
  };
  gmcli = inputPackageOrNull "gmcli" "default";
  keepbook = inputs.keepbook.packages.${system}.keepbook.overrideAttrs (_: {
    # Upstream checks currently depend on TS artifacts that are not built in Nix.
    doCheck = false;
  });
  repowise = pkgs.callPackage ../packages/repowise.nix {inherit inputs;};

  commonPkgs = lib.filter (pkg: lib.meta.availableOn pkgs.stdenv.hostPlatform pkg) (
    (import ../portable-packages.nix {inherit pkgs;})
    ++ [ccusage-fleet git-blame-rank keepbook repowise pkgs.git-sync]
    ++ lib.optionals (!isJayLenovo) [pkgs.bazel]
    ++ lib.optionals (gmcli != null) [gmcli]
    ++ lib.optionals (builtins.hasAttr "git-sync-rs" pkgs) [pkgs.git-sync-rs]
  );

  linuxOnly = with pkgs; [
    bubblewrap
    dex
    dpkg
    efibootmgr
    emacs-auto
    gparted
    inotify-tools
    iotop
    lshw
    mesa-demos
    patchelf
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
    wl-clipboard
  ];

  darwinOnly = with pkgs; [
    emacs
  ];
in {
  nixpkgs.config.allowBroken = true;

  # Centralize cargo's intermediate build artifacts (deps, incremental caches,
  # fingerprints — the bulk of target-dir bloat) under
  # ~/.cargo/build/<workspace-path-hash>, so deleting a worktree doesn't strand
  # gigabytes and cleanup has one place to prune. Final artifacts still land in
  # each project's target/.
  environment.variables.CARGO_BUILD_BUILD_DIR = "{cargo-cache-home}/build/{workspace-path-hash}";

  environment.systemPackages =
    commonPkgs
    ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux linuxOnly
    ++ lib.optionals pkgs.stdenv.hostPlatform.isDarwin darwinOnly;
}
