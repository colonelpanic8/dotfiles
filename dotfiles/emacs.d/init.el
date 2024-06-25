;; -*- no-byte-compile: t -*-

(setq native-comp-deferred-compilation-deny-list '("magit"))
(setq native-comp-always-compile t)
(setq load-no-native t)
(setq no-native-compile t)
(setq warning-minimum-level :emergency)

(setq package-enable-at-startup nil
      straight-use-package-by-default t
      straight-vc-git-default-protocol 'ssh)

(require 'use-package)
(setq use-package-enable-imenu-support t)
(setq use-package-always-ensure t)

(defvar imalison:do-benchmark nil)

(defun emacs-directory-filepath (filename)
  (concat (file-name-directory load-file-name) filename))

(load (emacs-directory-filepath "elpaca.el"))

(setq use-package-always-ensure t)

(let ((bench-file (concat (file-name-directory user-init-file) "benchmark.el")))
  (when (file-exists-p bench-file) (load bench-file)))

(use-package benchmark-init
  :if imalison:do-benchmark
  :demand t
  :config
  (setq max-specpdl-size 99999999))

(defvar imalison:kat-mode nil)
(setq custom-file "~/.emacs.d/custom-before.el")
(setq load-prefer-newer t)

;; If this isn't here and there's a problem with init, graphical emacs
;; is super annoying.
(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

(use-package transient
  :demand t)

;; See https://github.com/magit/magit/discussions/4997 . Without this magit is broken.
(use-package magit
  :demand t)

;; This seems to fix issues with helm not explicitly declaring its dependency on async
(use-package async :demand t)

(use-package s :demand t)

;; Without this, org can behave very strangely
(use-package org
  :ensure
  (org :type git :host github :repo "colonelpanic8/org-mode" :local-repo "org"
       :branch "my-main-2025"
       :depth full
       :build (:not autoloads)
       :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))
       :wait t))

(use-package dash :demand t)

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

(use-package s
  :ensure (:inherit t :wait t)
  :config
  (when (or (equal (s-trim (shell-command-to-string "whoami")) "kat")
            imalison:kat-mode)
    (let ((debug-on-error t))
      (org-babel-load-file
       (concat (file-name-directory load-file-name) "kat-mode.org")))))

(let ((debug-on-error t))
  (org-babel-load-file
   (concat (file-name-directory load-file-name) "README.org")))

;; (when imalison:do-benchmark (benchmark-init/deactivate))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
