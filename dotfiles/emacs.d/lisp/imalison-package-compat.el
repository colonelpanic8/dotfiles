;;; imalison-package-compat.el --- Local package build fixes -*- lexical-binding: t; -*-

;;; Commentary:
;; Smartparens and org-fc still use macros deprecated in Emacs 31.  Patch
;; build copies, leaving upstream checkouts clean for Elpaca updates.  Repair
;; existing builds at startup and repeat after Elpaca links a fresh build.

;;; Code:

(require 'elpaca)
(require 'lisp-mode)

(defconst imalison:package-compat-packages '(smartparens org-fc))

(defun imalison:package-compat-file (file)
  "Modernize obsolete conditional binding macros in build FILE.
Return non-nil if FILE changed.  Preserve legacy single-binding semantics,
skip strings and comments, and replace symlinks with patched build copies."
  (with-temp-buffer
    (insert-file-contents file)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (let (changed)
      (while (re-search-forward "(\\(if-let\\|when-let\\)\\_>" nil t)
        (let ((start (match-beginning 1))
              (end (match-end 1)))
          (unless (nth 8 (save-excursion (syntax-ppss start)))
            ;; Unlike the starred macros, the old macros also accept
            ;; (NAME VALUE) instead of ((NAME VALUE)).
            (forward-comment (point-max))
            (let* ((spec-start (point))
                   (spec (read (current-buffer)))
                   (spec-end (point)))
              (when (and (consp spec) (symbolp (car spec))
                         (proper-list-p spec) (= (length spec) 2))
                (goto-char spec-end)
                (insert ")")
                (goto-char spec-start)
                (insert "(")))
            (goto-char end)
            (insert "*")
            (setq changed t))))
      (when changed
        ;; Writing through Elpaca's symlink would dirty the source checkout.
        (when (file-symlink-p file) (delete-file file))
        (write-region (point-min) (point-max) file nil 'silent)
        (let ((bytecode (concat file "c")))
          (when (file-exists-p bytecode) (delete-file bytecode))))
      changed)))

(defun imalison:package-compat-directory (directory)
  "Apply local compatibility fixes to Lisp build files in DIRECTORY."
  (when (file-directory-p directory)
    (dolist (file (directory-files directory t "\\.el\\'"))
      (imalison:package-compat-file file))))

(defun imalison:package-compat-build (e)
  "Patch package E's build before autoload generation and compilation."
  (imalison:package-compat-directory (elpaca<-build-dir e))
  (elpaca-continue e))

(cl-defmethod elpaca-build-steps :around (e &optional _context)
  "Include local compatibility fixes for the affected packages."
  (let ((steps (cl-call-next-method)))
    (if (memq (elpaca<-id e) imalison:package-compat-packages)
        (elpaca-substitute-build-steps
         steps '(:after elpaca-build-link imalison:package-compat-build))
      steps)))

;; Installed packages can be activated without running their build steps.
(dolist (package imalison:package-compat-packages)
  (imalison:package-compat-directory
   (expand-file-name (symbol-name package) elpaca-builds-directory)))

(provide 'imalison-package-compat)
;;; imalison-package-compat.el ends here
