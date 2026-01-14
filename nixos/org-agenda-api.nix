# org-agenda-api.nix - Container and config for org-agenda-api
{ pkgs, inputs, system }:
let
  # Path to org-config.org in the dotfiles
  orgConfigOrg = ../dotfiles/emacs.d/org-config.org;

  # Tangle org-config.org and create a loader for the container
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

    # Copy all tangled files to output
    for f in work/org-config-*.el; do
      if [ -f "$f" ]; then
        cp "$f" $out/
      fi
    done

    # Create a loader that sets up paths and loads files in order
    cat > $out/custom-config.el << 'ELISP'
;;; custom-config.el --- Container config loader -*- lexical-binding: t; -*-

;; Set org directory for container
(defvar imalison:org-dir "/data/org")
(defvar imalison:shared-org-dir nil)

;; Helper function used by org-config
(defun imalison:join-paths (&rest paths)
  "Join PATHS together into a single path."
  (let ((result (car paths)))
    (dolist (p (cdr paths))
      (setq result (expand-file-name p result)))
    result))

;; Load tangled config files in order
(let ((config-dir (file-name-directory load-file-name)))
  (when (file-exists-p (expand-file-name "org-config-preface.el" config-dir))
    (load (expand-file-name "org-config-preface.el" config-dir)))
  ;; org-config-custom.el uses customize format (var value), convert to setq
  (when (file-exists-p (expand-file-name "org-config-custom.el" config-dir))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "org-config-custom.el" config-dir))
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (listp form) (symbolp (car form)))
                (set (car form) (eval (cadr form))))))
        (end-of-file nil))))
  (when (file-exists-p (expand-file-name "org-config-config.el" config-dir))
    (load (expand-file-name "org-config-config.el" config-dir))))

;; Capture templates for API
(setq org-agenda-api-capture-templates
      `(("gtd-todo"
         :name "GTD Todo"
         :template ("g" "GTD Todo" entry (file ,imalison:org-gtd-file)
                    (function (lambda () (imalison:make-org-todo-template :content "%^{Title}")))
                    :immediate-finish t)
         :prompts (("Title" :type string :required t)))
        ("scheduled-todo"
         :name "Scheduled Todo"
         :template ("s" "Scheduled" entry (file ,(imalison:join-paths imalison:org-dir "inbox.org"))
                    "* INBOX %^{Title}\nSCHEDULED: %^{When}t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                    :immediate-finish t)
         :prompts (("Title" :type string :required t)
                   ("When" :type date :required t)))
        ("deadline-todo"
         :name "Todo with Deadline"
         :template ("d" "Deadline" entry (file ,(imalison:join-paths imalison:org-dir "inbox.org"))
                    "* INBOX %^{Title}\nDEADLINE: %^{When}t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                    :immediate-finish t)
         :prompts (("Title" :type string :required t)
                   ("When" :type date :required t)))))
ELISP
  '';

  # Build customized org-agenda-api container
  orgAgendaApiContainer = inputs.org-agenda-api.lib.${system}.mkContainer {
    customElispFile = "${orgAgendaCustomConfig}/custom-config.el";
  };

in {
  org-agenda-custom-config = orgAgendaCustomConfig;
  org-agenda-api-container = orgAgendaApiContainer;
}
