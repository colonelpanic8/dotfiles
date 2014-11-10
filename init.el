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
;;                                                       Load Path Configuration
;; =============================================================================

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file) (load custom-file))

(add-to-list 'load-path "~/.emacs.d/lisp")

;; =============================================================================
;;                                                         ELPA/package.el/MELPA
;; =============================================================================

(require 'package)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t) 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar packages-appearance
  '(monokai-theme solarized-theme zenburn-theme base16-theme molokai-theme
    tango-2-theme gotham-theme color-theme-sanityinc-tomorrow smart-mode-line
    rainbow-delimiters ansi-color))

(defvar packages-essential
  '(epl use-package projectile flycheck ace-jump-mode helm helm-projectile popup
    smex magit auto-complete ido-ubiquitous mo-git-blame multiple-cursors
    yasnippet cl-lib flx-ido))

(defvar packages-other
  '(thingatpt+ latex-preview-pane auctex paredit inf-ruby undo-tree haskell-mode
	       exec-path-from-shell slime string-inflection yaml-mode sgml-mode
	       dired+ ctags ctags-update helm-gtags hackernews gitconfig-mode
	       aggressive-indent imenu+ weechat evil helm-ag xclip neotree))

(defvar packages-python '(jedi pymacs pytest sphinx-doc))
(defvar packages-scala '(scala-mode2 ensime))
(defvar packages-js '(js2-mode js3-mode web-beautify tern tern-auto-complete
			       slime-js skewer-mode skewer-reload-stylesheets))

(defun ensure-packages-installed (packages)
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p))))

(let ((packages (append packages-essential packages-python packages-scala packages-js
			packages-appearance packages-other)))
  (condition-case ex
      (ensure-packages-installed packages)
    ('error (package-refresh-contents)
	    (ensure-packages-installed packages) nil)))

;; =============================================================================
;;                                                                      Disables
;; =============================================================================

;; Disable the creation of backup files.
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)


(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(put 'set-goal-column 'disabled nil)
(auto-fill-mode -1)
  
;; =============================================================================
;;                                                         General Emacs Options
;; =============================================================================

;; Set path from shell.
(exec-path-from-shell-initialize)

;; This makes it so that emacs --daemon puts its files in ~/.emacs.d/server
;; (among other things)
(setq server-use-tcp t)

;; Give duplicate open buffers better titles.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Display line and column numbers in mode line.
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(setq visible-bell t)
(global-auto-complete-mode)
(setq ace-jump-mode-scope 'window)

;; Don't disable downcase and upcase region.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq c-subword-mode t)
(setq flyspell-issue-welcome-flag nil)
(latex-preview-pane-enable)

(add-hook 'after-init-hook '(lambda () (setq debug-on-error t)))
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; =============================================================================
;;                                                                    Mode Hooks
;; =============================================================================

(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))
(add-hook 'prog-mode-hook (lambda () (subword-mode t)))
(add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode t)))
(add-hook 'prog-mode-hook (lambda () (auto-complete-mode t)))
;; (add-hook 'prog-mode-hook (lambda () (highlight-lines-matching-regexp
;;                                  ".\\{81\\}" 'hi-blue)))

;; =============================================================================
;;                                               Navigation: helm/projectile/ido
;; =============================================================================

(helm-mode 1)
(ido-mode t)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(projectile-global-mode)
(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-enable-caching t)

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; This makes flx-ido much faster.
(setq gc-cons-threshold 20000000)
(autoload 'smex "smex"
  (global-set-key (kbd "M-x") 'smex))

;; =============================================================================
;;                                                                        Python
;; =============================================================================

(defun python-tabs () (setq tab-width 4
                            indent-tabs-mode t
                            python-indent-offset 4))

(defvar use-python-tabs nil)

(defun add-virtual-envs-to-jedi-server ()
  (let ((virtual-envs (get-virtual-envs)))
    (when virtual-envs (set (make-local-variable 'jedi:server-args)
                          (make-virtualenv-args virtual-envs)))))

(defun make-virtualenv-args (virtual-envs)
  (apply #'append (mapcar (lambda (env) `("-v" ,env)) virtual-envs)))

(defun get-virtual-envs ()
  (interactive)
  (condition-case ex
      (let (project-root (projectile-project-root))
        (cl-remove-if-not 'file-exists-p
                          (mapcar (lambda (env-suffix)
                                    (concat project-root env-suffix))
                                  '(".tox/py27/" "env" ".tox/venv/"))))
    ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex))))
         nil))

(require 'pytest)
(add-hook 'python-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'python-mode-hook (lambda () (if use-python-tabs python-tabs)))
(add-hook 'python-mode-hook (lambda () (subword-mode t)))
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'add-virtual-envs-to-jedi-server)

(setq jedi:complete-on-dot t)

;; Macros
(fset 'ipdb "import ipdb; ipdb.set_trace()")
(fset 'main "if __name__ == '__main__':")
(fset 'sphinx-class ":class:`~")

;; =============================================================================
;;                                                                         Scala
;; =============================================================================

(add-to-list 'load-path "~/Projects/scala-mode2")
(require 'ensime)
(require 'scala-mode2)
;; (load "~/.emacs.d/lisp/ensime-imenu.el")
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook '(lambda ()
  (require 'whitespace)

  ;; clean-up whitespace at save
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; turn on highlight. To configure what is highlighted, customize
  ;; the *whitespace-style* variable. A sane set of things to
  ;; highlight is: face, tabs, trailing
  (whitespace-mode)))

;; =============================================================================
;;                                                                    JavaScript
;; =============================================================================

(setq js-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(defun skewer-mode-all ()
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

;; (require 'slime-js)
;; (slime-js-init)
;; (require 'setup-slime-js)
;; (global-set-key [f5] 'slime-js-reload)
;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (slime-js-minor-mode 1)))

(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
            (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)))

;; =============================================================================
;;                                                                         C/C++
;; =============================================================================

;;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))

;; key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;; =============================================================================
;;                                                                           TeX
;; =============================================================================

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq-default TeX-master nil)

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

(defun get-buffer-name()
  (interactive)
  (file-relative-name (buffer-file-name)))

(defun message-buffer-name()
  (interactive)
  (message (get-buffer-name)))

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

;; Miscellaneous
(define-key lisp-mode-shared-map (kbd "C-c C-c") 'eval-defun)
(global-unset-key (kbd "C-o")) ;; Avoid collision with tmux binding.


(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-;") 'ace-jump-mode)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c +") 'message-buffer-name)
(global-set-key (kbd "C-c <") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-f") 'find-function-at-point)
(global-set-key (kbd "C-c C-r") 'eval-and-replace)
(global-set-key (kbd "C-c C-s") 'sudo-edit)
(global-set-key (kbd "C-c SPC") (lambda () (interactive)
				  (if current-prefix-arg (helm-global-mark-ring) (helm-mark-ring))))
(global-set-key (kbd "C-c e") 'os-copy)
(global-set-key (kbd "C-c g") 'jedi:goto-definition) ;; Should be python only
(global-set-key (kbd "C-c j") 'ace-jump-mode) ;; This is needed for terminal emacs.
(global-set-key (kbd "C-c t") 'pytest-one)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x C-c") 'kill-emacs)
(global-set-key (kbd "C-x C-i") 'imenu)
(global-set-key (kbd "C-x C-r") (lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x f") 'helm-projectile-find-file)
(global-set-key (kbd "C-x r t") 'mc/edit-lines)
(global-set-key (kbd "C-x w") 'whitespace-mode)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; Multiple Cursors

(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;; =============================================================================
;;                                                                    Appearance
;; =============================================================================

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
(defvar light-themes '(monokai))

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
(defvar fonts '("Inconsolata-12" ))

(defun set-my-font-for-frame (frame)
  (condition-case exp
      (set-default-font (random-choice fonts) nil t)
    ('error (package-refresh-contents)
	    (set-default-font "monaco-11" nil t) nil)))

(defun remove-fringe-and-hl-line-mode (&rest stuff)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (set-fringe-mode 0)
  (set-my-font-for-frame nil)
  (setq hl-line-mode nil))

(advice-add 'load-theme :after #'remove-fringe-and-hl-line-mode)

;; enable to set theme based on time of day.
(run-at-time "00:00" 3600 'set-theme)

(add-hook 'after-make-frame-functions 'set-my-font-for-frame)
