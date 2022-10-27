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

(setq custom-file "~/.emacs.d/custom-before.el")
(setq load-prefer-newer t)

;; If this isn't here and there's a problem with init, graphical emacs
;; is super annoying.
(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

;; This seems to fix issues with helm not explicitly declaring its dependency on async
(use-package async :demand t)

;; Without this, org can behave very strangely
(use-package org
  :defer t
  :init
  (progn
    ;; Taken from https://github.com/raxod502/radian/blob/master/radian-emacs/radian-org.el
    (defun radian--org-git-version ()
      "Return the abbreviated SHA for the Org Git repo."
      (let ((default-directory (concat user-emacs-directory
                                       "straight/repos/org/")))
        (if (executable-find "git")
            (with-temp-buffer
              ;; Returns the shortest prefix of the SHA for HEAD that is
              ;; unique, down to a minimum of 4 characters (see
              ;; git-rev-parse(1)).
              (call-process "git" nil '(t nil) nil
                            "rev-parse" "--short" "HEAD")
              (if (> (buffer-size) 0)
                  (string-trim (buffer-string))
                ;; This shouldn't happen, unless somehow Org is not
                ;; actually a Git repo.
                "revision unknown"))
          ;; This also shouldn't happen, because how would you have
          ;; gotten Org in the first place, then? But the real world
          ;; sucks and we have to account for stuff like this.
          "git not available")))
    (defalias #'org-git-version #'radian--org-git-version)
    (defun org-release () "N/A")
    (provide 'org-version)
    (with-eval-after-load 'org
      (defalias #'org-git-version #'radian--org-git-version))))

(let ((debug-on-error t))
  (org-babel-load-file
   (concat (file-name-directory load-file-name) "README.org")))

(when imalison:do-benchmark (benchmark-init/deactivate))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
