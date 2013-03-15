;; -----------------------------------------------------------------------------
;; Ivan Malison
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;; -----------------------------------------------------------------------------

(setq user-full-name    "Ivan Malison")
(setq user-mail-address "<IvanMalison@gmail.com>")

;; =============================================================================
;;                                                       Load Path Configuration
;; =============================================================================

(let ((default-directory "~/.emacs.d/lisp/"))
      (normal-top-level-add-subdirs-to-load-path))
(setq load-path (cons "~/.emacs.d/elpa" load-path))

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

;; Unique buffer names dependent on file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(add-hook 'python-mode-hook (lambda () (setq show-trailing-whitespace t)))
(setq visible-bell t)

;; Multi-lining for python.
(require 'multi-line-it)

;; Display line and column numbers in mode line.
(line-number-mode t)
(column-number-mode t)

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








;; pymacs
(require 'pymacs)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(pymacs-load "ropemacs" "rope-")
(whitespace-mode t)



; tab display width of 4 columns by default
; (throw everything at the wall, and eventually something will stick...)
(setq-default tab-width 4)  ; Normal emacs tab-width
; (setq-default c-basic-offset 2) ; python-mode.el setting
(setq-default py-indent-offset 4) ; Use Tabs, not spaces
(setq-default py-indent-offset 4) ; emacs-for-python setting
(setq-default py-smart-indentation nil) ; Don't try to guess tab width



(setq c-subword-mode t)
(put 'upcase-region 'disabled nil)
(defun change-major-mode-hook () (modify-syntax-entry ?_ "_"))

(put 'downcase-region 'disabled nil)

