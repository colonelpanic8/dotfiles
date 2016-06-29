;; -*- no-byte-compile: t -*-
(require 'package)
(package-initialize)

(setq load-prefer-newer t)

(setq custom-file "~/.emacs.d/custom-before.el")
(when (file-exists-p custom-file) (load custom-file))

;; If this isn't here and there's a problem with init, graphical emacs
;; is super annoying.
(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

(org-babel-load-file
 (concat (file-name-directory load-file-name) "README.org"))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
