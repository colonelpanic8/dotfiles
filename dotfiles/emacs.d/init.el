;; -*- no-byte-compile: t; lexical-binding: t; -*-

(setq native-comp-deferred-compilation-deny-list '("magit"))
(setq native-comp-always-compile t)
(setq load-no-native t)
(setq no-native-compile t)

(defvar imalison:do-benchmark nil)

(defun emacs-directory-filepath (filename)
  (concat (file-name-directory load-file-name) filename))

(load-file (expand-file-name "elpaca-installer.el" user-emacs-directory))
(elpaca elpaca-use-package (elpaca-use-package-mode))
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
  (org :type git :host github :repo "colonelpanic8/org-mode" :local-repo "org"
       :branch "my-main-2025"
       :depth full
       :build (:not autoloads)
       :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))
       :wait t))

(elpaca-wait)

;; Install transient early to prevent built-in version from loading
(use-package transient
  :ensure (:host github :repo "magit/transient" :wait t)
  :demand t)
(elpaca-wait)

(when (or (equal (s-trim (shell-command-to-string "whoami")) "kat")
            imalison:kat-mode)
    (let ((debug-on-error t))
      (org-babel-load-file
       (concat (file-name-directory load-file-name) "kat-mode.org"))))

(let ((debug-on-error t))
  (org-babel-load-file
   (expand-file-name "README.org" user-emacs-directory)))

;; (when imalison:do-benchmark (benchmark-init/deactivate))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
