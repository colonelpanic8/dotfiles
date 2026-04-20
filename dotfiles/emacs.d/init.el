;; -*- no-byte-compile: t; lexical-binding: t; -*-

(setq native-comp-deferred-compilation-deny-list '("magit"))
(setq native-comp-always-compile t)
(setq load-no-native t)
(setq no-native-compile t)

(defvar imalison:do-benchmark nil)

(defun emacs-directory-filepath (filename)
  (expand-file-name filename user-emacs-directory))

(load-file (expand-file-name "elpaca-installer.el" user-emacs-directory))

;; Elpaca's initial queue logger can fire during self-bootstrap before its
;; helper is callable under --debug-init. Keep command logging, but skip the
;; fragile initial-queue logger so startup diagnostics reach the real config.
(setq elpaca-log-functions '(elpaca-log-command-query))
;; Default hosted git clones to SSH (e.g., git@github.com:owner/repo.git).
(setq elpaca-order-defaults (plist-put elpaca-order-defaults :protocol 'ssh))

(defun imalison:existing-executable (&rest candidates)
  (seq-find #'file-executable-p (delq nil candidates)))

(defun imalison:emacs-bin-directory ()
  (let ((emacsclient (or (executable-find "emacsclient")
                         (when invocation-directory
                           (expand-file-name "emacsclient" invocation-directory))
                         (when invocation-directory
                           (expand-file-name "../../../../bin/emacsclient"
                                             invocation-directory)))))
    (when (and emacsclient (file-executable-p emacsclient))
      (directory-file-name (file-name-directory emacsclient)))))

(defun imalison:emacsclient-executable ()
  (imalison:existing-executable
   (executable-find "emacsclient")
   (when invocation-directory
     (expand-file-name "emacsclient" invocation-directory))
   (when invocation-directory
     (expand-file-name "../../../../bin/emacsclient" invocation-directory))))

;; GUI Emacs launched from the app bundle may not inherit a PATH that contains
;; the matching emacsclient binary.
(when-let ((emacs-bin (imalison:emacs-bin-directory)))
  (add-to-list 'exec-path emacs-bin)
  (setenv "PATH" (concat emacs-bin path-separator (or (getenv "PATH") ""))))
(setq emacsclient-program-name
      (imalison:emacsclient-executable))
(setq with-editor-emacsclient-executable
      (imalison:emacsclient-executable))

(defun imalison:elpaca-menu-local-repos (request &optional item)
  (when (eq request 'index)
    (let ((root elpaca-sources-directory))
      (cl-labels
          ((item-info
             (pkg)
             (let* ((name (symbol-name pkg))
                    (dir (expand-file-name name root))
                    (git-dir (expand-file-name ".git" dir)))
               (when (file-directory-p git-dir)
                 (with-temp-buffer
                   (when (zerop (call-process "git" nil t nil "-C" dir "config" "--get" "remote.origin.url"))
                     (let ((remote (string-trim (buffer-string))))
                       (when (not (string-empty-p remote))
                         (list :source "Local repos"
                               :recipe (list :package name
                                             :repo remote
                                             :local-repo name))))))))))
        (if item
            (item-info item)
          (cl-loop for path in (directory-files root nil "^[^.]" t)
                   for pkg = (intern path)
                   for info = (item-info pkg)
                   when info
                   collect (cons pkg info)))))))

(add-to-list 'elpaca-menu-functions #'imalison:elpaca-menu-local-repos t)

(elpaca elpaca-use-package (elpaca-use-package-mode))
(elpaca-wait)
(setq use-package-enable-imenu-support t)
(setq use-package-always-ensure t)

(defvar imalison:kat-mode nil)
(setq custom-file "~/.emacs.d/custom-before.el")
(setq load-prefer-newer t)

;; If this isn't here and there's a problem with init, graphical emacs
;; is super annoying.
(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

;;The packages in this section are used to as utilities in the rest of this init file.
;;Ensure they are installed/activated first.
(use-package s
  :ensure (:wait t)
  :demand t)

(use-package dash
  :ensure (:wait t)
  :demand t
  :config
  (progn (dash-enable-font-lock)))

;; Some split packages fall through the active menus in this config. Give
;; Elpaca an explicit source so startup doesn't get stuck on recipe lookup or
;; stale branch-mapped clones.
(elpaca `(queue :host github :repo "emacs-straight/queue"))
(elpaca `(git-commit :host github :repo "magit/magit"
                     :files ("lisp/git-commit.el" "lisp/git-commit-pkg.el")))
(elpaca `(magit-section :host github :repo "magit/magit"
                        :files ("lisp/magit-section.el" "lisp/magit-section-pkg.el")))

(use-package gh
  :defer t
  :ensure (:host github :repo "IvanMalison/gh.el"))

(use-package shut-up
  :config
  (defun imalison:shut-up-around (function &rest args)
    (shut-up (apply function args))))

(use-package parse-csv :demand t)

(use-package emit
  :ensure (emit :type git :host github :repo "colonelpanic8/emit")
  :demand t
  :config
  (progn
    (emit-prefix-selector imalison:mark-ring mark-ring)
    (emit-prefix-selector imalison:shell-command-on-region
      imalison:copy-shell-command-on-region
      imalison:shell-command-on-region-replace
      imalison:jq-replace)

    (defun imalison:jq-replace (start end)
      (interactive (region-if-active-otherwise-buffer))
      (imalison:shell-command-on-region-replace start end "jq ."))
    (emit-compose
      imalison:copy-eval-last-sexp kill-new prin1-to-string eval-last-sexp)

    (emit-prefix-selector imalison:eval-last-sexp
      eval-region-or-last-sexp
      imalison:copy-eval-last-sexp)))

(use-package request :defer t)

;; Without this, org can behave very strangely
(use-package org
  :ensure
  ;; Keep Org on the historical local checkout name to match the existing
  ;; source directory under ~/.emacs.d/straight/repos/org.
  (org :type git :host github
       :repo ("colonelpanic8/org-mode" . "org")
       :branch "my-main-2025"
       :depth full
       :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))
       :wait t))

(elpaca-wait)

(require 'org)

(defun imalison:load-literate-file (org-file)
  (let ((el-file (concat (file-name-sans-extension org-file) ".el")))
    ;; Prefer the tangled file on normal startup and only re-tangle when the
    ;; Org source changed.
    (if (and (file-exists-p el-file)
             (not (file-newer-than-file-p org-file el-file)))
        (load-file el-file)
      (org-babel-load-file org-file))))

(defun imalison:load-kat-mode ()
  (let ((debug-on-error t))
    (imalison:load-literate-file
     (emacs-directory-filepath "kat-mode.org"))))

;; Install transient early to prevent built-in version from loading
;; Workaround: overriding-text-conversion-style is void on pgtk builds (no
;; HAVE_TEXT_CONVERSION) but transient's .elc compiled on X11 has static-if
;; expanded to reference it directly
(unless (boundp 'overriding-text-conversion-style)
  (defvar overriding-text-conversion-style nil))
(use-package transient
  :ensure (:host github :repo "magit/transient" :wait t)
  :demand t)
(elpaca-wait)

;; Magit's split packages are compiled separately; make them available before
;; the larger config queue reaches magit itself.
(use-package git-commit
  :ensure nil
  :defer t)

(use-package magit-section
  :ensure nil
  :defer t)

(elpaca-wait)

(let ((debug-on-error t))
  (imalison:load-literate-file
   (expand-file-name "README.org" user-emacs-directory)))

(when (or (equal (s-trim (shell-command-to-string "whoami")) "kat")
          imalison:kat-mode)
  ;; Machine-specific overrides can reuse packages declared in README once
  ;; Elpaca has activated the main init queue.
  (add-hook 'elpaca-after-init-hook #'imalison:load-kat-mode))

;; (when imalison:do-benchmark (benchmark-init/deactivate))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
