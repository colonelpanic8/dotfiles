{
  config,
  lib,
  nixos,
  ...
}: let
  # Replicate the useful part of rcm/rcup:
  # - dotfiles live in <dotfiles-worktree>/dotfiles (no leading dots in the repo)
  # - links in $HOME add a leading '.' to the first path component
  # - link files individually so unmanaged state can coexist (e.g. ~/.cabal/store)
  #
  # Use out-of-store symlinks so editing the repo updates config immediately
  # without a rebuild/switch (only link *placement* is managed by HM).
  oos = config.lib.file.mkOutOfStoreSymlink;

  # Where the checked-out repo lives at runtime (activation time).
  # Keep this outside individual home directories so links work for every
  # managed user on a shared machine.
  worktreeRoot = nixos.config.dotfiles-worktree or "/srv/dotfiles";
  worktreeDotfiles = "${worktreeRoot}/dotfiles";

  # Use the flake source for enumeration (pure), but point links at the worktree.
  srcDotfiles = ../dotfiles;
  srcConfig = srcDotfiles + "/config";
  srcCodex = srcDotfiles + "/codex";

  excludedTop = [
    # Managed by nix-shared/home-manager/codex-generated-skills.nix so
    # config.toml can be generated from shared and machine-local fragments.
    "codex"
    # Managed by Nix directly (PATH/fpath), not meant to appear as ~/.lib.
    "lib"
    # Avoid colliding with HM-generated xdg.configFile entries for now.
    "config"
    # Handled as a single directory symlink below.
    "emacs.d"
  ];

  firstComponent = rel: let
    parts = lib.splitString "/" rel;
  in
    lib.elemAt parts 0;

  isExcluded = rel: lib.elem (firstComponent rel) excludedTop;

  listFilesRec = dir: let
    entries = builtins.readDir dir;
    names = builtins.attrNames entries;
    go = name: let
      ty = entries.${name};
      path = dir + "/${name}";
    in
      if ty == "directory"
      then map (p: "${name}/${p}") (listFilesRec path)
      else [name];
  in
    lib.concatLists (map go names);

  managedRelFiles =
    lib.filter (rel: !(isExcluded rel)) (listFilesRec srcDotfiles);

  mkManaged = rel:
    lib.nameValuePair ".${rel}" {
      source = oos "${worktreeDotfiles}/${rel}";
    };

  configEntries = builtins.readDir srcConfig;
  configDirExclusions = [
    # Home Manager writes generated fontconfig fragments under this tree.
    "fontconfig"
    # Home Manager's gtk module writes settings.ini and gtk.css here.
    "gtk-3.0"
  ];
  configDirNames =
    lib.filter
    (name: configEntries.${name} == "directory" && !(lib.elem name configDirExclusions))
    (builtins.attrNames configEntries);
  mkConfigDir = name:
    lib.nameValuePair name {
      source = oos "${worktreeDotfiles}/config/${name}";
    };
in {
  imports = [
    ../nix-shared/home-manager/codex-generated-skills.nix
  ];

  home.file =
    builtins.listToAttrs (map mkManaged managedRelFiles);

  xdg.configFile =
    builtins.listToAttrs (map mkConfigDir configDirNames);

  myModules.codexGeneratedSkills.enable = true;
  myModules.codexGeneratedSkills.sourceCodexDir = "${srcCodex}";
  # Point the Codex module at the live worktree (e.g. /srv/dotfiles) like the
  # links above, not its ~/dotfiles default. Without this, ~/.codex/AGENTS.md
  # and ~/.codex/skills/* dangle when the checkout lives outside ~/dotfiles.
  myModules.codexGeneratedSkills.worktreeCodexDir = "${worktreeDotfiles}/codex";

  # Home Manager directory links for .emacs.d resolve through the store on this
  # machine, which breaks Elpaca's writable state under ~/.emacs.d/elpaca.
  # Manage placement here instead so ~/.emacs.d always points at the live
  # worktree checkout.
  home.activation.linkEmacsDotdir = lib.hm.dag.entryAfter ["writeBoundary"] ''
    if [ -L "$HOME/.emacs.d" ] || [ ! -e "$HOME/.emacs.d" ]; then
      rm -f "$HOME/.emacs.d"
      ln -s "${worktreeDotfiles}/emacs.d" "$HOME/.emacs.d"
    else
      echo "Skipping ~/.emacs.d relink because it is not a symlink" >&2
    fi
  '';
}
