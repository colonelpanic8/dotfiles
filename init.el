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
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t) 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(color-theme cl-lib ctags ctags-update flycheck ensime 
                                  multiple-cursors latex-preview-pane pytest epl
                                  starter-kit-bindings zenburn-theme jedi tern
                                  starter-kit magit ido-ubiquitous monokai-theme
                                  idle-highlight-mode find-file-in-project smex
                                  paredit inf-ruby undo-tree rainbow-delimiters
                                  solarized-theme tern-auto-complete scala-mode2
                                  gitconfig-mode starter-kit-ruby mo-git-blame
                                  auto-complete project-root popup web-beautify
                                  js2-mode js3-mode sphinx-doc ansi-color pytest
                                  exec-path-from-shell base16-theme slime
                                  string-inflection yasnippet yaml-mode)
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

;; Set path from shell.
(exec-path-from-shell-initialize)

;; Disable the creation of backup files.
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Fuck auto fill mode
(auto-fill-mode -1)

;; This makes it so that emacs --daemon puts its files in ~/.emacs.d/server
(setq server-use-tcp t)

(put 'set-goal-column 'disabled nil)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Evvvillll
(setq use-dialog-box nil)


;; Set the default font for emacs.
;;(set-face-attribute 'default t :font "Deja Vu")
;;(set-frame-font "Deja Vu Sans Mono" t t)

;;(set-face-attribute 'default nil :height 80)

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
(require 'find-file-in-project)
(setq ffip-limit 9999999999)

(setq js-indent-level 2)

(defun no-auto-fill-hook () (auto-fill-mode -1))

(add-hook 'prog-mode-hook 'no-auto-fill-hook)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook  (lambda () (subword-mode 1)))
(add-hook 'js-mode (lambda () (subword-mode 1)))
;; (add-hook 'prog-mode-hook (lambda () (highlight-lines-matching-regexp
;;                                  ".\\{81\\}" 'hi-blue)))


(setq flyspell-issue-welcome-flag nil)

(add-hook 'after-init-hook
          '(lambda () (setq debug-on-error t)))

(latex-preview-pane-enable)

(require 'project-root)
(setq project-roots
      `(("Tox Project"
         :root-contains-files ("tox.ini")
         :filename-regex (regexify-ext-list '(py)))
        ("Python Project"
         :root-contains-files (".git" "setup.py")
         :filename-regex (regexify-ext-list '(py)))
        ("Git project"
         :root-contains-files (".git"))))

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file) (load custom-file))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; =============================================================================
;;                                                                          Misc
;; =============================================================================

(defun get-buffer-name()
  (interactive)
  (file-relative-name (buffer-file-name)
                      (expand-file-name (with-project-root
                                            (cdr project-details)))))

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

;; =============================================================================
;;                                                                          tmux
;; =============================================================================

(defun tmux-copy (&optional b e) 
  (interactive "r")
  (shell-command-on-region b e "cat | tmux loadb -"))

(defun tmux-copy-buffer-name (&optional b e)
  (interactive "r")
  (shell-command (concat "echo " (shell-quote-argument (ffip-get-buffer-name))
                         " | tmux loadb -")))

;; =============================================================================
;;                                                                        Python
;; =============================================================================

;; Multi-lining for python.
(require 'multi-line-it)
(require 'pytest)

(add-hook 'python-mode-hook (lambda () (setq show-trailing-whitespace t)))

(defun python-tabs () (setq tab-width 4
                            indent-tabs-mode t
                            python-indent-offset 4))

(defvar use-python-tabs nil)

(add-hook 'python-mode-hook (lambda () (if use-python-tabs python-tabs)))
(add-hook 'python-mode-hook (lambda () (subword-mode 1)))
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(defun add-virtual-envs-to-jedi-server ()
  (let ((virtual-envs (get-virtual-envs)))
    (when virtual-envs (set (make-local-variable 'jedi:server-args)
                          (make-virtualenv-args virtual-envs)))))

(defun make-virtualenv-args (virtual-envs)
  (apply #'append (mapcar (lambda (env) `("-v" ,env)) virtual-envs)))

(defun get-virtual-envs ()
  (interactive)
  (condition-case ex
      (let ((project-root (with-project-root (cdr project-details))))
        (cl-remove-if-not 'file-exists-p
                          (mapcar (lambda (env-suffix)
                                    (concat project-root env-suffix))
                                  '(".tox/py27/" "env" ".tox/venv/"))))
    ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex))))
         nil))


(add-hook 'python-mode-hook 'add-virtual-envs-to-jedi-server)

;; Macros
(fset 'ipdb "import ipdb; ipdb.set_trace()")
(fset 'main "if __name__ == '__main__':")
(fset 'sphinx-class ":class:`~")

(global-auto-complete-mode)

;; Macros
(fset 'ipdb "import ipdb; ipdb.set_trace()")

;; =============================================================================
;;                                                                         Scala
;; =============================================================================

(require 'ensime)
(require 'scala-mode2)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook '(lambda ()
  (require 'whitespace)

  ;; clean-up whitespace at save
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; turn on highlight. To configure what is highlighted, customize
  ;; the *whitespace-style* variable. A sane set of things to
  ;; highlight is: face, tabs, trailing
  (whitespace-mode)
))
;; =============================================================================
;;                                                           Custom Key Bindings
;; =============================================================================

;; Miscellaneous
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-unset-key (kbd "C-o"))
(global-set-key (kbd "C-x w") 'whitespace-mode)
(global-set-key (kbd "C-x C-r") (lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-M-;") 'comment-dwim)
(global-set-key (kbd "C-c t") 'pytest-one)
(global-set-key (kbd "C-c e") 'os-copy)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x C-c") 'kill-emacs)
(global-set-key (kbd "C-c +") 'message-buffer-name)

(global-set-key "\C-cg" 'jedi:goto-definition)

(global-unset-key (kbd "C-o"))

;; Multiple Cursors

(global-set-key (kbd "C-x r t") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-all-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)

(global-set-key (kbd "C-_") 'undo)
(global-set-key (kbd "C--") 'undo)

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

(if (and (eq system-type 'darwin) window-system)
    (load-theme 'solarized-dark t) (load-theme 'monokai t))
(require 'color-theme)
(require 'whitespace)
(require 'rainbow-delimiters)

;; make whitespace-mode use just basic coloring
(setq whitespace-style (quote (spaces tabs newline ;;space-mark
                                      tab-mark newline-mark)))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46]) 
        (tab-mark 9 [9655 9] [92 9])))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
