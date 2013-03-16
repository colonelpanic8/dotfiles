;; =============================================================================
;; Ivan Malison
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;; =============================================================================

(setq user-full-name    "Ivan Malison")
(setq user-mail-address "<IvanMalison@gmail.com>")

;; =============================================================================
;;                                                       Load Path Configuration
;; =============================================================================

(let ((default-directory "~/.emacs.d/lisp/"))
      (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/elpa")

(message load-path)

;; =============================================================================
;;                                                         General Emacs Options
;; =============================================================================

;; Disable the creation of backup files.
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Enable ido mode.
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Give duplicate open buffers better titles.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(add-hook 'python-mode-hook (lambda () (setq show-trailing-whitespace t)))
(setq visible-bell t)

;; Display line and column numbers in mode line.
(line-number-mode t)
(column-number-mode t)

;; Don't disable downcase and upcase region.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Change the behavior of M-<arrow> so that it stops on underscores.
(defun change-major-mode-hook () (modify-syntax-entry ?_ "_"))
(setq c-subword-mode t)

;; =============================================================================
;;                                                                        Python
;; =============================================================================

;; Emacs for python.
(load-file "~/.emacs.d/emacs-for-python/epy-init.el")

;; Multi-lining for python.
;;(require 'multi-line-it)

;; Set tabs to be four spaces wide in python mode.
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode t)
        (setq tab-width 4)
        (setq python-indent 4)))

;; =============================================================================
;;                                                           Custom Key Bindings
;; =============================================================================

;; Fast cursor movement in vertical direction with Meta.
(global-set-key (kbd "M-<down>") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-<up>") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "ESC <down>") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "ESC <up>") (lambda () (interactive) (previous-line 5)))

;; Macros
(fset 'ipdb "import ipdb; ipdb.set_trace()")

;; Miscellaneous
(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\C-xw" 'whitespace-mode)
(global-set-key "\C-x\C-r" (lambda () (interactive) (revert-buffer t t)))
(global-set-key "\C-x\C-i" 'increase-left-margin)
(global-set-key "\C-x\C-d" 'decrease-left-margin)
(global-set-key "\C-c\C-c" 'comment-region)

;; =============================================================================
;;                                                                          ELPA
;; =============================================================================

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; =============================================================================
;;                                                                       Flymake
;; =============================================================================

(require 'flymake)
(require 'flymake-cursor)

;; Customize flymake colors.
(custom-set-faces
 '(flymake-errline ((((class color)) (:background "DarkViolet"))))
 '(flymake-warnline ((((class color)) (:underline "Orange")))))

(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                    'flymake-create-temp-inplace))
        (local-file (file-relative-name
                     temp-file
                     (file-name-directory buffer-file-name))))
   (list "pyflakes" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pylint-init))

; Load flymake on non-temp buffers
(add-hook 'python-mode-hook
	  (lambda () (unless (eq buffer-file-name nil) (flymake-mode 1))))

(require 'yasnippet)
(require 'whitespace)
(require 'rainbow-delimiters)
