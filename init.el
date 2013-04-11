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
(let ((default-directory "~/.emacs.d/elpa/"))
      (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")
(load-theme 'solarized-dark t)

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

;; Disable the menu bar.
(menu-bar-mode -1)

;; find-file-in-project
(setq ffip-limit 9999999999)

;; =============================================================================
;;                                                                       Flymake
;; =============================================================================

(require 'flymake)
(require 'flymake-cursor)

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

;; =============================================================================
;;                                                                        Python
;; =============================================================================

;; Multi-lining for python.
(require 'multi-line-it)

(defun python-tabs ()
  (setq tab-width 4
        indent-tabs-mode t
        python-indent-offset 4))
(python-tabs)

;; Yelp always uses tabs.
(add-hook 'python-mode-hook 'python-tabs)

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

;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; Add in your own as you wish:
;; (defvar my-packages '(starter-kit starter-kit-bindings)
;;   "A list of packages to ensure are installed at launch.")

;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; ;; Add in your own as you wish:
;; (defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings)
;;   "A list of packages to ensure are installed at launch.")

;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;    (package-install p)))

;; =============================================================================
;;                                                                 elisp Helpers
;; =============================================================================

(defun plist-to-alist (the-plist)
  (defun get-tuple-from-plist (the-plist)
    (when the-plist
      (cons (car the-plist) (cadr the-plist))))

  (let ((alist '()))
    (while the-plist
      (add-to-list 'alist (get-tuple-from-plist the-plist))
      (setq the-plist (cddr the-plist)))
  alist))

;; =============================================================================
;;                                                                    Appearance
;; =============================================================================

(require 'color-theme)
(require 'whitespace)
(require 'rainbow-delimiters)

;; make whitespace-mode use just basic coloring
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46]) 
        (tab-mark 9 [9655 9] [92 9])))

;; (set-face-background 'mode-line "black")
;; (set-face-foreground 'mode-line "white")
;; (set-face-background 'mode-line-inactive "black")

;; Customize font-faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:background "DarkViolet"))))
 '(flymake-warnline ((((class color)) (:underline "Orange"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "magenta")))))

;; =============================================================================
;;                                                                  Starter Kits
;; =============================================================================

(load-file "~/.emacs.d/emacs-for-python/epy-init.el")

(setq skeleton-pair nil) ;; This stuff sucks.

;; =============================================================================
;;                                                                     Customize
;; =============================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#e4e4e4" "#d70000" "#5f8700" "#af8700" "#0087ff" "#af005f" "#00afaf" "#808080"])
 '(ansi-term-color-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(background-color "#ffffd7")
 '(background-mode light)
 '(cursor-color "#626262")
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" default)))
 '(fci-rule-color "#383838")
 '(foreground-color "#626262"))
