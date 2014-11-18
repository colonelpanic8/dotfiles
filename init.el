;; =============================================================================
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;; =============================================================================

(setq user-full-name
      (replace-regexp-in-string "\n$" "" (shell-command-to-string
                                          "git config --get user.email")))
(setq user-mail-address
      (replace-regexp-in-string "\n$" "" (shell-command-to-string
                                          "git config --get user.name")))

;; =============================================================================
;;                                                                  GUI Disables
;; =============================================================================

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; =============================================================================
;;;;                                                     Load Path Configuration
;; =============================================================================

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file) (load custom-file))

(add-to-list 'load-path "~/.emacs.d/lisp")

;; =============================================================================
;;;;                                                       ELPA/package.el/MELPA
;; =============================================================================

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(defun ensure-packages-installed (packages)
  (unless package-archive-contents
    (package-refresh-contents))
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         package
       (progn (message (format "Installing package %s." package))
              (package-install package))))
   packages))

(package-initialize)
(require 'benchmark-init)
(ensure-packages-installed '(epl use-package))
(require 'use-package)
(put 'use-package 'lisp-indent-function 1) ;; reduce indentation for use-package

;;; =============================================================================
;;                                                                      Disables
;; =============================================================================

;; Disable the creation of backup files.
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Why is this necessary again?
(defconst emacs-tmp-dir
  (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)


(put 'set-goal-column 'disabled nil)
(auto-fill-mode -1)
(setq indent-tabs-mode nil)

;; No hsplits. EVER.
(defun split-horizontally-for-temp-buffers () (split-window-horizontally))

(add-hook 'temp-buffer-setup-hook 'split-horizontally-for-temp-buffers)
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; =============================================================================
;;                                                          Config Free Packages
;; =============================================================================

(defvar packages-essential
  '(popup auto-complete ido-ubiquitous mo-git-blame multiple-cursors
    yasnippet cl-lib flx-ido))

(defvar packages-other
  '(thingatpt+ latex-preview-pane paredit inf-ruby undo-tree
    exec-path-from-shell slime string-inflection yaml-mode sgml-mode
    dired+ ctags ctags-update helm-gtags hackernews gitconfig-mode
    aggressive-indent imenu+ weechat evil helm-ag xclip neotree
    magit-gh-pulls diminish gist))

(defvar packages-appearance
  '(monokai-theme solarized-theme zenburn-theme base16-theme molokai-theme
    tango-2-theme gotham-theme color-theme-sanityinc-tomorrow smart-mode-line
    rainbow-delimiters ansi-color))

(ensure-packages-installed packages-essential)
(ensure-packages-installed packages-other)
(ensure-packages-installed packages-appearance)
  
;; =============================================================================
;;                                                         General Emacs Options
;; =============================================================================

;; Set path from shell.
(exec-path-from-shell-initialize)

;; This makes it so that emacs --daemon puts its files in ~/.emacs.d/server
;; (among other things)
(setq server-use-tcp t)

;; Display line and column numbers in mode line.
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(setq visible-bell t)
(setq uniquify-buffer-name-style 'forward)

;; Don't disable downcase and upcase region.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq c-subword-mode t)
(setq flyspell-issue-welcome-flag nil)

(add-hook 'after-init-hook '(lambda () (setq debug-on-error t)))
(add-hook 'after-init-hook #'flycheck-mode)

(require 'tramp)
(setq tramp-default-method "ssh")

(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :config
  (progn 
    (setq ace-jump-mode-scope 'window))
  :bind (("C-;" . ace-jump-mode)
	 ;; This is needed for terminal emacs.
	 ("C-c j" . ace-jump-mode)))

(use-package flycheck
  :ensure t
  :config
  (progn
    (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
    (global-flycheck-mode)
    (diminish 'flycheck-mode)))
  
(use-package haskell-mode
  :ensure t
  :commands haskell-mode
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)))

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (progn
    (add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode t)))))

(use-package magit
  :ensure t
  :commands magit-status
  :bind (("C-x g" . magit-status))
  :config
  (diminish 'magit-auto-revert-mode))

(use-package auto-complete
  :ensure t
  :commands auto-complete-mode
  :config
  (diminish 'auto-complete-mode)
  :init
  (add-hook 'prog-mode-hook (lambda () (auto-complete-mode t))))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :bind (("C-@" . er/expand-region)))

;; =============================================================================
;;                                                        Programming Mode Hooks
;; =============================================================================

(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))
(add-hook 'prog-mode-hook (lambda () (subword-mode t)))
;; (add-hook 'prog-mode-hook (lambda () (highlight-lines-matching-regexp
;;                                  ".\\{81\\}" 'hi-blue)))

;; =============================================================================
;;                                               Navigation: helm/projectile/ido
;; =============================================================================

(use-package helm
  :ensure t
  :bind (("M-y" . helm-show-kill-ring))
  :config
  (progn
    (helm-mode 1)
    (diminish 'helm-mode)))

(use-package projectile
  :ensure t
  :config
  (progn
    (setq projectile-enable-caching t)
    (projectile-global-mode)
    (helm-projectile-on)
    (diminish 'projectile-mode))
  :bind (
         ("C-x f" . helm-projectile-find-file)) 
  :init
  (progn
    (use-package flx
      :ensure t
      :config
      (progn
        ;; disable ido faces to see flx highlights.
        (flx-ido-mode 1)
        ;; This makes flx-ido much faster.
        (setq gc-cons-threshold 20000000)
        (setq ido-use-faces nil))
      :init
      (progn
        (use-package flx-ido
          :ensure t)))
    (use-package helm-projectile
      :ensure t
      :config
      (progn (helm-projectile-on)))))

(ido-mode t)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

(use-package smex :ensure t)

;; =============================================================================
;;;;                                                                  emacs-lisp
;; =============================================================================

(use-package elisp-slime-nav
  :ensure t
  :commands elisp-slime-nav-mode
  :config
  (diminish 'elisp-slime-nav-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t))))


(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (setq imenu-space-replacement nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; *\\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))
(define-key lisp-mode-shared-map (kbd "C-c C-c") 'eval-defun)
(define-key lisp-mode-shared-map (kbd "C-c C-f") 'find-function-at-point)
(define-key lisp-mode-shared-map (kbd "C-c C-r") 'eval-and-replace)

;; =============================================================================
;;                                                                        Python
;; =============================================================================

(defvar use-python-tabs nil)

(defun python-tabs ()
  (setq tab-width 4 indent-tabs-mode t python-indent-offset 4))

(defun add-virtual-envs-to-jedi-server ()
  (let ((virtual-envs (get-virtual-envs)))
    (when virtual-envs (set (make-local-variable 'jedi:server-args)
			    (make-virtualenv-args virtual-envs)))))

(defun make-virtualenv-args (virtual-envs)
  (apply #'append (mapcar (lambda (env) `("-v" ,env)) virtual-envs)))

(defun get-virtual-envs ()
  (condition-case ex
      (let ((project-root (projectile-project-root)))
	(cl-remove-if-not 'file-exists-p
			  (mapcar (lambda (env-suffix)
				    (concat project-root env-suffix))
				  '(".tox/py27/" "env" ".tox/venv/"))))
    ('error
     (message (format "Caught exception: [%s]" ex))
     (setq retval (cons 'exception (list ex))))
    nil))

(defun message-virtual-envs ()
  (interactive)
  (message "%s" (get-virtual-envs)))

(use-package python
  :commands python-mode
  :mode ("\\.py\\'" . python-mode)
  :config
  (progn
    ;; macros
    (fset 'ipdb "import ipdb; ipdb.set_trace()")
    (fset 'main "if __name__ == '__main__':")
    (fset 'sphinx-class ":class:`~")
  :init
  (progn
    (use-package jedi
      :commands jedi:goto-definition
      :config (setq jedi:complete-on-dot t)
      :ensure t
      :bind ("C-c g" . jedi:goto-definition))
    (use-package pytest
      :ensure t
      :bind ("C-c t" . pytest-one))
    (use-package pymacs :ensure t)
    (use-package sphinx-doc :ensure t)
    (add-hook 'python-mode-hook (lambda () (setq show-trailing-whitespace t)))
    (add-hook 'python-mode-hook (lambda () (if use-python-tabs (python-tabs))))
    (add-hook 'python-mode-hook (lambda () (subword-mode t)))
    (add-hook 'python-mode-hook #'jedi:setup)
    (add-hook 'python-mode-hook #'add-virtual-envs-to-jedi-server))))

;; =============================================================================
;;                                                                         Scala
;; =============================================================================

;; (load "~/.emacs.d/lisp/ensime-imenu.el")

(use-package scala-mode2
  :init
  (progn (add-hook 'scala-mode-hook
		   (lambda ()
		     (require 'whitespace)
		     (make-local-variable 'before-save-hook)
		     (add-hook 'before-save-hook 'whitespace-cleanup)
		     (whitespace-mode))))
  :config
  (progn
    (use-package ensime
      :config (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
      :ensure t)
    (setq scala-indent:align-parameters t))
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sc\\'" . scala-mode))
  :ensure t)

;; =============================================================================
;;                                                                    JavaScript
;; =============================================================================

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (progn (setq js-indent-level 2))
  :bind
  (("C-c b" . web-beautify-js)
   ("C-c b" . web-beautify-js))
  :init
  (progn
    (use-package skewer-mode
      :ensure t
      :commands skewer-mode)
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
    (add-hook 'js2-mode-hook 'skewer-mode)
    (use-package tern
      :ensure t
      :config
      (progn (tern-ac-setup))
      :init
      (progn
	(use-package tern-auto-complete :ensure t
	  :commands tern-ac-setup)))))

(defvar packages-js '(js2-mode js3-mode web-beautify tern tern-auto-complete
		      slime-js skewer-mode skewer-reload-stylesheets))

(add-hook 'css-mode-hook #'skewer-css-mode)
(add-hook 'html-mode-hook #'skewer-html-mode)

(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
            (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;; =============================================================================
;;                                                                         C/C++
;; =============================================================================

(use-package helm-gtags
  :ensure t
  :config (custom-set-variables
	   '(helm-gtags-path-style 'relative)
	   '(helm-gtags-ignore-case t)
	   '(helm-gtags-auto-update t))
  :bind
  (("M-t" . helm-gtags-find-tag)
   ("M-r" . helm-gtags-find-rtag)
   ("M-s" . helm-gtags-find-symbol)
   ("M-g M-p" . helm-gtags-parse-file)
   ("C-c <" . helm-gtags-previous-history)
   ("C-c >" . helm-gtags-next-history)
   ("M-," . helm-gtags-pop-stack))
  :init
  (progn
    ;;; Enable helm-gtags-mode
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)))

;; =============================================================================
;;                                                                           TeX
;; =============================================================================

(use-package tex-site
  :ensure auctex
  :config
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-save-query nil)
    (setq-default TeX-master nil)))

;; =============================================================================
;;                                                                     functions
;; =============================================================================

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file (as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun get-buffer-name ()
  (interactive)
  (file-relative-name (buffer-file-name)))

(defun message-buffer-name ()
  (interactive)
  (message (get-buffer-name)))

(defun frame-exists ()
  (cl-find-if (lambda (frame)
             (assoc 'display (frame-parameters frame)))
           (frame-list)))

(defun make-frame-if-none-exists ()
  (unless (frame-exists)
    (make-frame-on-display (getenv "DISPLAY"))))

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

(defun tmux-copy (&optional b e) 
  (interactive "r")
  (shell-command-on-region b e "cat | tmux loadb -"))

(defun tmux-copy-buffer-name (&optional b e)
  (interactive "r")
  (shell-command (concat "echo " (shell-quote-argument (ffip-get-buffer-name))
                         " | tmux loadb -")))

(defun eval-and-replace ()
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; =============================================================================
;;                                                           Custom Key Bindings
;; =============================================================================

(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-t)
         ("C->" . mc/mark-next-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-all-like-this)
         ("C-x r t" . mc/edit-lines)))
          
;; Miscellaneous
(global-unset-key (kbd "C-o")) ;; Avoid collision with tmux binding.
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-c +") 'message-buffer-name)

(global-set-key (kbd "C-c C-s") 'sudo-edit)
(global-set-key (kbd "C-c SPC") (lambda () (interactive)
				  (if current-prefix-arg (helm-global-mark-ring)
                                    (helm-mark-ring))))
(global-set-key (kbd "C-c e") 'os-copy)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x C-c") 'kill-emacs)
(global-set-key (kbd "C-x C-i") 'imenu)
(global-set-key (kbd "C-x C-r") (lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x w") 'whitespace-mode)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-z") 'zap-to-char)

(fset 'global-set-key-to-use-package
      (lambda (&optional arg) "Keyboard macro." (interactive "p")
        (kmacro-exec-ring-item
	 (quote ([1 67108896 19 100 6 23 40 19 41 return
		    backspace 32 46 6 4] 0 "%d")) arg)))

;; =============================================================================
;;                                                                    Appearance
;; =============================================================================

;; No splash screen please... jeez
(setq inhibit-startup-screen t)
(blink-cursor-mode -1)

;; make whitespace-mode use just basic coloring
(setq whitespace-style (quote (spaces tabs newline ;;space-mark
                                      tab-mark newline-mark)))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46]) 
        (tab-mark 9 [9655 9] [92 9])))

(defun colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; smart-mode-line
(sml/setup)
(sml/apply-theme 'respectful)

;; =============================================================================
;;                                                                        Themes
;; =============================================================================

;; Choose random theme:
;; (defvar dark-themes '(monokai molokai solarized-dark base16-default))
;; (defvar light-themes '(zenburn solarized-light))

(defvar dark-themes '(monokai))
(defvar light-themes '(solarized-light))

(defun random-choice (choices)
  (nth (random (length choices)) choices))

(defun get-appropriate-theme ()
  (let ((hour
         (string-to-number (format-time-string "%H"))))
    (if (or (< hour 8) (> hour 18))
        (random-choice dark-themes) (random-choice light-themes))))

(setq current-theme nil)

(defun set-theme ()
  (interactive)
  (let ((appropriate-theme (get-appropriate-theme)))
        (if (eq appropriate-theme current-theme)
            nil
          (progn (load-theme appropriate-theme t)
                 (setq current-theme appropriate-theme)))))

;;(defvar fonts '("DejaVu Sans Mono-10" "monaco-11" "Inconsolata-12" "menlo-10"))
(defvar fonts '("monaco-10"))

(defun set-my-font-for-frame (frame)
  (condition-case exp
      (set-frame-font (random-choice fonts) nil t)
    ('error (package-refresh-contents)
	    (set-frame-font "monaco-11" nil t) nil)))

(defun remove-fringe-and-hl-line-mode (&rest stuff)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (set-fringe-mode 0)
  (set-my-font-for-frame nil)
  (setq hl-line-mode nil))

(if (or
     (and (>= emacs-major-version 24)
          (>= emacs-minor-version 4))
     (>= emacs-major-version 25))
    (advice-add 'load-theme :after #'remove-fringe-and-hl-line-mode)
  (defadvice load-theme (after name activate)
    (remove-fringe-and-hl-line-mode)))


;; enable to set theme based on time of day.
(run-at-time "00:00" 3600 'set-theme)

;; This is needed because you can't set the font at daemon start-up.
(add-hook 'after-make-frame-functions 'set-my-font-for-frame)
