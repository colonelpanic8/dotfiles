;; =============================================================================
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;; =============================================================================

(setq user-full-name
      (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.email")))
(setq user-mail-address
      (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.name")))

;; =============================================================================
;;                                                       Load Path Configuration
;; =============================================================================

(if (not (file-exists-p "~/.emacs.d/elpa"))
    (make-directory "~/.emacs.d/elpa"))
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/")
(require 'patches)

;; =============================================================================
;;                                                                          ELPA
;; =============================================================================

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")) 
(package-initialize)

(defvar my-packages '(color-theme ctags ctags-update flymake mo-git-blame
                                  multiple-cursors latex-preview-pane
                                  starter-kit-bindings starter-kit-ruby
                                  starter-kit magit ido-ubiquitous
                                  find-file-in-project idle-highlight-mode
                                  paredit inf-ruby undo-tree rainbow-delimiters
                                  smex solarized-theme zenburn-theme
                                  scala-mode2 ensime monokai-theme
                                  gitconfig-mode jedi flymake-cursor pytest)
  "Packages that must be installed at launch.")

(defun ensure-package-installed (packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every package not installed."
  ;; fetch the list of packages available 
  (unless package-archive-contents
    (package-refresh-contents))
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         package
       (progn (message (format "Installing package %s." package))
              (package-install package))))
   packages))

(ensure-package-installed my-packages)

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

(setq visible-bell t)

;; Display line and column numbers in mode line.
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)

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

(setq js-indent-level 2)

(defun no-auto-fill-hook () (auto-fill-mode -1))

(add-hook 'html-mode-hook 'no-auto-fill-hook)
(add-hook 'text-mode-hook 'no-auto-fill-hook)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

(setq flyspell-issue-welcome-flag nil)

(add-hook 'after-init-hook
          '(lambda () (setq debug-on-error t)))

(latex-preview-pane-enable)
;; (add-hook 'latex-mode-hook (lambda () (global-set-key (kbd "M-n") ))

;; =============================================================================
;;                                                                          Misc
;; =============================================================================

(defun ffip-get-buffer-name()
  (interactive)
  (file-relative-name (buffer-file-name) (expand-file-name (ffip-project-root))))

(defun message-buffer-name()
  (interactive)
  (message (ffip-get-buffer-name)))

(defun os-copy (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "source ~/.zshrc; cat | smart_copy"))

(defun os-paste ()
  (interactive)
  (insert (shell-command-to-string "source ~/.zshrc; ospaste")))

(defun all-copy (&optional b e)
  (interactive "r")
  (os-copy b e)
  (tmux-copy b e)
  (kill-ring-save b e))

(defun open-pdf ()
  (interactive)
  (let ( (pdf-file (replace-regexp-in-string "\.tex$" ".pdf" buffer-file-name)))
    (shell-command (concat "open " pdf-file))))

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
          (lambda () (unless (or (eq buffer-file-name nil) (eq (file-name-directory buffer-file-name) nil)) (flymake-mode 1))))

;; =============================================================================
;;                                                                        Python
;; =============================================================================

;; Multi-lining for python.
(require 'multi-line-it)
(require 'emacs-testify)
(require 'pytest)

(add-hook 'python-mode-hook (lambda () (setq show-trailing-whitespace t)))

(defun python-tabs () (setq tab-width 4
                            indent-tabs-mode t
                            python-indent-offset 4))

(defvar use-python-tabs nil)

(add-hook 'python-mode-hook (lambda () (if use-python-tabs python-tabs)))
(add-hook 'python-mode-hook (lambda () (subword-mode 1)))

;; =============================================================================
;;                                                                    JavaScript
;; =============================================================================

(add-hook 'js-mode-common-hook (lambda () (subword-mode 1)))
(add-hook 'js-mode (lambda () (subword-mode 1)))

;; =============================================================================
;;                                                                         Scala
;; =============================================================================

(add-hook 'scala-mode-hook (lambda () (subword-mode 1)))

;; =============================================================================
;;                                                                  Starter Kits
;; =============================================================================

;;(load-file "~/.emacs.d/emacs-for-python/epy-init.el")

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
(fset 'main "if __name__ == '__main__':")

;; Miscellaneous
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-x C-r") (lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c C-c") 'comment-dwim)
(global-set-key (kbd "C-c t") 'testify-run-test)
(global-set-key (kbd "C-c C-o") 'testify-run-case)
(global-set-key (kbd "C-c w") 'all-copy)
(global-set-key (kbd "C-c e") 'os-copy)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-c t") 'testify-run-test)
(global-set-key (kbd "C-c C-t") 'testify-run-case)
(global-set-key (kbd "C-x C-c") 'kill-emacs)
(global-set-key (kbd "C-c +") 'message-buffer-name)

;; Something will occasionally override this binding.
(global-set-key "\C-cg" 'jedi:goto-definition)

(global-unset-key (kbd "C-o"))

;; Macros
(fset 'ipdb "import ipdb; ipdb.set_trace()")

;; Multiple Cursors

(global-set-key (kbd "C-x r t") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-all-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)

(global-set-key (kbd "C-_") 'undo)
(global-set-key (kbd "C--") 'undo)

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

(load-theme 'monokai t)
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
;;                                                                     Customize
;; =============================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((use-python-tabs . t) (python-indent . 4) (whitespace-line-column . 80) (lexical-binding . t)))))

