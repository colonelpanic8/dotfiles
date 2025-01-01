;; -*- no-byte-compile: t -*-

(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq native-comp-deferred-compilation-deny-list '("magit"))
(setq native-comp-always-compile t)
(setq load-no-native t)
(setq no-native-compile t)
(setq warning-minimum-level :emergency)
(setq straight-disable-native-compile t)

;; This is a workaround for an issue in emacs28 with symlinks. See https://github.com/radian-software/straight.el/issues/701
(defun my-patch-package-find-file-visit-truename (oldfun &rest r)
  (let ((find-file-visit-truename nil))
    (apply oldfun r)))

(advice-add #'straight--build-autoloads :around
            #'my-patch-package-find-file-visit-truename)

(setq package-enable-at-startup nil
      straight-use-package-by-default t
      straight-vc-git-default-protocol 'ssh)
(straight-use-package 'use-package)
(require 'use-package)
(setq use-package-enable-imenu-support t)
(setq use-package-ensure-function 'straight-use-package-ensure-function)

(defvar imalison:do-benchmark nil)

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

;; See https://github.com/magit/magit/discussions/4997 . Without this magit is broken.
(use-package magit
  :demand t)

;; This seems to fix issues with helm not explicitly declaring its dependency on async
(use-package async :demand t)

;; Without this, org can behave very strangely
(use-package org
  :straight
  (org :type git :host github :repo "colonelpanic8/org-mode" :local-repo "org"
       :branch "my-main-2025"
       :depth full :pre-build (straight-recipes-org-elpa--build) :build
       (:not autoloads) :files
       (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*")))
  :defer t)

(use-package dash :demand t)

(let ((debug-on-error t))
  (org-babel-load-file
   (concat (file-name-directory load-file-name) "README.org")))

(when (or (equal (s-trim (shell-command-to-string "whoami")) "kat")
          imalison:kat-mode)
  (let ((debug-on-error t))
    (org-babel-load-file
     (concat (file-name-directory load-file-name) "kat-mode.org"))))

(when imalison:do-benchmark (benchmark-init/deactivate))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
