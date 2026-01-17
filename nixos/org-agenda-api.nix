# org-agenda-api.nix - Tangled org-config for org-agenda-api container
{ pkgs, inputs, system }:
let
  # Path to org-config.org in the dotfiles
  orgConfigOrg = ../dotfiles/emacs.d/org-config.org;

  # Tangle org-config.org to produce elisp files
  orgAgendaCustomConfig = pkgs.runCommand "org-agenda-custom-config" {
    buildInputs = [ pkgs.emacs-nox ];
  } ''
    mkdir -p $out
    mkdir -p work

    # Copy org file to writable location (tangle writes to same directory)
    cp ${orgConfigOrg} work/org-config.org

    # Tangle org-config.org
    emacs --batch \
      --eval '(require (quote org))' \
      --eval '(org-babel-tangle-file "work/org-config.org")'

    # Copy all tangled files to output, stripping :straight keywords
    # (straight.el is not available in the minimal container Emacs)
    for f in work/org-config-*.el; do
      if [ -f "$f" ]; then
        # Remove :straight nil and :straight t from use-package declarations
        sed -e 's/:straight nil//g' -e 's/:straight t//g' "$f" > "$out/$(basename "$f")"
      fi
    done
  '';

in {
  org-agenda-custom-config = orgAgendaCustomConfig;
}
