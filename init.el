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
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/")
(load-theme 'zenburn t)

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
(global-linum-mode t)

;; Don't disable downcase and upcase region.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Don't allow arrow keys
(require 'no-easy-keys)
(no-easy-keys 1)

;; Change the behavior of M-<arrow> so that it stops on underscores.
(defun change-major-mode-hook () (modify-syntax-entry ?_ "_"))
(setq c-subword-mode t)

;; Disable the menu bar.
(menu-bar-mode -1)

;; find-file-in-project
(setq ffip-limit 9999999999)

(setq js-indent-level 2)

(defun no-auto-fill-hook () (auto-fill-mode -1))

(add-hook 'html-mode-hook 'no-auto-fill-hook)
(add-hook 'text-mode-hook 'no-auto-fill-hook)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

(setq flyspell-issue-welcome-flag nil)

(add-hook 'after-init-hook
          '(lambda () (setq debug-on-error t)))

;; =============================================================================
;;                                                                          Misc
;; =============================================================================

(defun ffip-get-buffer-name()
  (interactive)
  (path-relative-to-base-path (buffer-file-name) (expand-file-name (ffip-project-root))))

(defun message-buffer-name()
  (interactive)
  (message (ffip-get-buffer-name)))

(defun path-relative-to-base-path(file-path base-path)
  (mapconcat 'identity (list-diff (path-to-list base-path) (path-to-list file-path)) "/"))

(defun path-to-list(path)
  (let ((path-list (split-string path "/")))
    (if (= (length (car (last path-list))) 0) (butlast path-list) path-list)))

(defun list-diff(shorter longer)
  (cond ((not shorter) longer)
        ((string= (car shorter) (car longer)) (list-diff (cdr shorter) (cdr longer)))
        (t (throw 'error "longer does not match shorter"))))

(defun remote-os-copy (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "source ~/.zshrc; cat | linux_nc_paste_to_remote_clipboard"))

(defun os-copy (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "pbcopy"))

;; =============================================================================
;;                                                                          tmux
;; =============================================================================

(defun tmux-copy (&optional b e) 
  (interactive "r")
  (shell-command-on-region b e "cat | tmux loadb -"))

(defun tmux-copy-buffer-name (&optional b e)
  (interactive "r")
  (shell-command (concat "echo " (shell-quote-argument (ffip-get-buffer-name)) " | tmux loadb -")))

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
(require 'emacs-testify)

(add-hook 'python-mode-hook (lambda () (subword-mode 1)))

;; =============================================================================
;;                                                           Custom Key Bindings
;; =============================================================================

;; Fast cursor movement in vertical direction with Meta.
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "ESC n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "ESC p") (lambda () (interactive) (previous-line 5)))

;; Macros
(fset 'ipdb "import ipdb; ipdb.set_trace()")

;; Miscellaneous
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x w") 'whitespace-mode)
(global-set-key (kbd "C-c w") 'tmux-copy)
(global-set-key (kbd "C-x C-r") (lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c C-c") 'comment-dwim)
(global-set-key (kbd "C-c t") 'testify-run-test)
(global-set-key (kbd "C-c C-o") 'testify-run-case)
(global-set-key (kbd "C-c w") 'tmux-copy)
(global-set-key (kbd "C-c e") 'remote-os-copy)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-c t") 'testify-run-test)
(global-set-key (kbd "C-c C-t") 'testify-run-case)
(global-set-key (kbd "C-c +") (lambda () (interactive) (message (ffip-get-buffer-name))))


;; Something will occasionally override this binding.
(global-set-key "\C-cg" 'rope-goto-definition)

;; Macros
(fset 'ipdb "import ipdb; ipdb.set_trace()")

;; Multiple Cursors

(global-set-key (kbd "C-x r t") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-all-like-this)

;; =============================================================================
;;                                                                          ELPA
;; =============================================================================

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")) 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
 '(safe-local-variable-values (quote ((python-indent . 4) (whitespace-line-column . 80) (lexical-binding . t)))))
