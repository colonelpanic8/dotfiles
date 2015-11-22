;; -*- lexical-binding: t; -*-
;; =============================================================================
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;; =============================================================================


(setq user-full-name
      (replace-regexp-in-string "\n$" "" (shell-command-to-string
                                          "git config --get user.name")))
(setq user-mail-address
      (replace-regexp-in-string "\n$" "" (shell-command-to-string
                                          "git config --get user.email")))

(defun emacs24_4-p ()
  (or (and (>= emacs-major-version 24)
           (>= emacs-minor-version 4))
      (>= emacs-major-version 25)))

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

;; =============================================================================
;;                                                       Load Path Configuration
;; =============================================================================

(defvar machine-custom "~/.emacs.d/this-machine.el")
(setq custom-file "~/.emacs.d/custom-before.el")
(when (file-exists-p custom-file) (load custom-file))
(defvar custom-after-file "~/.emacs.d/custom-after.el")
(when (file-exists-p machine-custom) (load machine-custom))

;; =============================================================================
;;                                                                  GUI Disables
;; =============================================================================

 ;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; =============================================================================
;;                                                                 byte-compiler
;; =============================================================================

;; These silence the byte compiler.
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-context-switch-command nil)
(defvar ido-cur-list nil)
(defvar inherit-input-method nil)
(defvar grep-find-ignored-files nil)
(defvar grep-find-ignored-directories nil)

;; =============================================================================
;;                                                                      Security
;; =============================================================================

(defvar imalison:secure t)

(defun imalison:use-https-and-tls ()
  (setq tls-checktrust t)
  (let ((trustfile
         (replace-regexp-in-string
          "\\\\" "/"
          (replace-regexp-in-string
           "\n" ""
           (shell-command-to-string "python -m certifi")))))
    (setq tls-program
          (list
           (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                   (if (eq window-system 'w32) ".exe" "") trustfile)))))

(defun imalison:test-security ()
  (interactive)
  (let ((bad-hosts
       (loop for bad
             in `("https://wrong.host.badssl.com/"
                  "https://self-signed.badssl.com/")
             if (condition-case e
                    (url-retrieve
                     bad (lambda (retrieved) t))
                  (error nil))
             collect bad)))
  (if bad-hosts
      (error (format "tls misconfigured; retrieved %s ok"
                     bad-hosts))
    (url-retrieve "https://badssl.com"
                  (lambda (retrieved) t)))))

(when imalison:secure (imalison:use-https-and-tls))

;; =============================================================================
;;                                                         ELPA/package.el/MELPA
;; =============================================================================

(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives '("elpa" . "https://tromey.com/elpa/") t)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(setq package-archives nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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
(ensure-packages-installed '(epl use-package))

;; use-package is only needed at compile time.
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(use-package diminish)
(use-package bind-key)
(use-package bug-hunter)

(use-package benchmark-init
  ;; this variable has to be set in custom-before.el
  :if (and (boundp 'do-benchmark) do-benchmark))

;; =============================================================================
;;                                                          Config Free Packages
;; =============================================================================

(defvar packages-eager
  '(popup cl-lib
    xclip dired+ ctags ctags-update aggressive-indent imenu+ neotree diminish
    gist))

(ensure-packages-installed packages-eager)

;; =============================================================================
;;                                                                      Disables
;; =============================================================================

(setq visible-bell nil)
(setq sentence-end-double-space nil)

;; Disable the creation of backup files.
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)

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

;; No popup frames.
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)
(setq confirm-nonexistent-file-or-buffer nil)

;; No prompt for killing a buffer with processes attached.
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(setq tooltip-use-echo-area t)

(setq use-dialog-box nil)

(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;; =============================================================================
;;                                                                     functions
;; =============================================================================

(defun display-prefix (arg)
  "Display the value of the raw prefix arg."
  (interactive "P")
  (message "%s" arg))

(defmacro imalison:prefix-alternatives (name &rest alternatives)
  `(defun ,name (arg)
     (interactive "P")
     (setq arg (or arg '(1)))
     (setq prefix-value (car arg))
     (setq function
           (cond
            ,@(progn
               (setq last-multiple 1)
               (cl-loop for alternative in alternatives
                        collect `((eq prefix-value ,last-multiple) (quote ,alternative))
                        do (setq last-multiple (* last-multiple 4))))))
     (setq function (or function (car alternatives))) ; Set a default value for function
     (setq current-prefix-arg nil)
     (call-interactively function)))

(defun imalison:uuid ()
  (interactive)
  (s-replace "\n" "" (shell-command-to-string "uuid")))

(defun imalison:disable-linum-mode ()
  (linum-mode 0))

(defun imalison:disable-smartparens-mode ()
  (smartparens-mode 0))

(defun imalison:insert-uuid ()
  (interactive)
  (insert (imalison:uuid)))

(defmacro suppress-messages (&rest forms)
  `(flet ((message (&rest r) nil))
     ,@forms))

(defun imalison:compare-int-list (a b)
  (when (and a b)
    (cond ((> (car a) (car b)) 1)
          ((< (car a) (car b)) -1)
          (t (imalison:compare-int-list (cdr a) (cdr b))))))

(defun imalison:get-lat-long ()
  (condition-case ex
      (mapcar 'string-to-number (s-split "," (s-trim (shell-command-to-string "whereami"))))
    (error (list 37.7879312624533 -122.402388853402))))

(defun get-date-created-from-agenda-entry (agenda-entry)
  (org-time-string-to-time
   (org-entry-get (get-text-property 1 'org-marker agenda-entry) "CREATED")))

(defmacro defvar-setq (name value)
  (if (boundp name)
      `(setq ,name ,value)
    `(defvar ,name ,value)))

(defun imalison:imenu-prefix-flattened (index)
  (let ((flattened (imalison:flatten-imenu-index (cdr index))))
    (cl-loop for sub-item in flattened
             collect
             `(,(concat (car index) "." (car sub-item)) . ,(cdr sub-item)))))

(defun imalison:flatten-imenu-index (index)
  (let ((cdr-is-index (listp (cdr index))))
    (cond ((not (stringp (car index))) (cl-mapcan #'imalison:flatten-imenu-index index))
          (cdr-is-index (imalison:imenu-prefix-flattened index))
          (t (list index)))))

(defun imalison:make-imenu-index-flat ()
  (let ((original-imenu-function imenu-create-index-function))
    (setq imenu-create-index-function
          (lambda ()
            (imalison:flatten-imenu-index
             (funcall original-imenu-function))))))

(defmacro defvar-if-non-existent (name value)
  (unless (boundp name)
    `(defvar ,name ,value)))

(defun eval-region-or-last-sexp ()
  (interactive)
  (if (region-active-p) (call-interactively 'eval-region)
    (call-interactively 'eval-last-sexp)))

(defun undo-redo (&optional arg)
  (interactive "P")
  (if arg (undo-tree-redo) (undo-tree-undo)))

(defun up-list-region ()
  (interactive)
  (up-list) (set-mark-command nil) (backward-sexp))

(defun up-list-back ()
  (interactive)
  (up-list) (backward-sexp))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION). With the prefix argument UNFILL,
unfill it instead."
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (list (if current-prefix-arg 'unfill) t)))
    (let ((fill-column (if unfill (point-max) fill-column)))
      (fill-paragraph nil region)))

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

(defun frame-exists ()
  (cl-find-if
   (lambda (frame)
     (assoc 'display (frame-parameters frame))) (frame-list)))

(defun imalison:copy-shell-command-on-region (start end command)
  (interactive (list (region-beginning) (region-end)
                     (read-shell-command "Shell command on region: ")))
  (let ((original-buffer (current-buffer)))
    (with-temp-buffer
      (let ((temp-buffer (current-buffer)))
        (with-current-buffer original-buffer
          (shell-command-on-region start end command temp-buffer))
        (kill-ring-save (point-max) (point-min))))))

(defun imalison:shell-command-on-region-replace (start end command)
  (interactive (list (region-beginning) (region-end)
                     (read-shell-command "Shell command on region: ")))
  (shell-command-on-region start end command nil t))

(defun imalison:shell-command-on-region (arg)
  (interactive "P")
  (call-interactively (if arg 'imalison:shell-command-on-region-replace
    'imalison:copy-shell-command-on-region)))

(defun make-frame-if-none-exists ()
  (let* ((existing-frame (frame-exists)))
    (if existing-frame
        existing-frame
      (make-frame-on-display (getenv "DISPLAY")))))

(defun make-frame-if-none-exists-and-focus ()
  (make-frame-visible (select-frame (make-frame-if-none-exists))))

(defun copy-buffer-file-name ()
  (interactive)
  (add-string-to-kill-ring (file-name-nondirectory (buffer-file-name))))

(defun copy-buffer-file-path ()
  (interactive)
  (add-string-to-kill-ring (file-relative-name (buffer-file-name) (projectile-project-root))))

(defun add-string-to-kill-ring (string)
  (with-temp-buffer
    (insert string)
    (kill-ring-save (point-max) (point-min))))

(defun os-copy-string (string)
  (with-temp-buffer
    (insert string)
    (os-copy (beginning-of-buffer) (end-of-buffer))))

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
  (let ( (pdf-file (replace-regexp-in-string
                    "\.tex$" ".pdf" buffer-file-name)))
    (shell-command (concat "open " pdf-file))))

(defun tmux-copy (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "cat | tmux loadb -"))

(defun eval-and-replace ()
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun flatten-imenu-index (index)
  (cl-mapcan
   (lambda (x)
     (if (listp (cdr x))
         (cl-mapcar (lambda (item)
                      `(,(concat (car x) "/" (car item)) . ,(cdr item)))
                    (flatten-imenu-index (cdr x)))
       (list x))) index))

(defun flatten-imenu-index-function (function)
  (lambda () (flatten-imenu-index (funcall function))))

(defun flatten-current-imenu-index-function ()
  (setq imenu-create-index-function
        (flatten-imenu-index-function imenu-create-index-function)))

(defun notification-center (title message)
  (flet ((encfn (s) (encode-coding-string s (keyboard-coding-system))))
    (shell-command
     (format "osascript -e 'display notification \"%s\" with title \"%s\"'"
             (encfn message) (encfn title)))))

(defun growl-notify (title message)
  (shell-command (format "grownotify -t %s -m %s" title message)))

(defun notify-send (title message)
  (shell-command (format "notify-send -u critical %s %s" title message)))

(defvar notify-function
  (cond ((eq system-type 'darwin) 'notification-center)
        ((eq system-type 'gnu/linux) 'notify-send)))

(defun project-root-of-file (filename)
  "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (file-truename
   (let ((dir (file-truename filename)))
     (or (--reduce-from
          (or acc
              (let* ((cache-key (format "%s-%s" it dir))
                     (cache-value (gethash cache-key
                                           projectile-project-root-cache)))
                (if cache-value
                    (if (eq cache-value 'no-project-root)
                        nil
                      cache-value)
                  (let ((value (funcall it dir)))
                    (puthash cache-key (or value 'no-project-root)
                             projectile-project-root-cache)
                    value))))
          nil
          projectile-project-root-files-functions)
         (if projectile-require-project-root
             (error "You're not in a project")
           default-directory)))))

;; =============================================================================
;;                                                         General Emacs Options
;; =============================================================================

(global-auto-revert-mode)

;; This makes it so that emacs --daemon puts its files in ~/.emacs.d/server
;; (setq server-use-tcp t)

;; Display line and column numbers in mode line.
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(setq visible-bell t)
(show-paren-mode 1)

;; Make buffer names unique.
(setq uniquify-buffer-name-style 'forward)

;; We want closures
(setq lexical-binding t)

;; Don't disable commands...
(setq disabled-command-function nil)

;; Make forward word understand camel and snake case.
(setq c-subword-mode t)
(global-subword-mode)

;; Preserve pastes from OS when saving a new item to the kill
;; ring. Why wouldn't this be enabled by default?
(setq save-interprogram-paste-before-kill t)

(setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows 'bar)

(if nil ;; Causing too many annoying issues
    (add-hook 'after-init-hook '(lambda () (setq debug-on-error t))))

;; Make mouse scrolling less jumpy.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(eval-after-load 'subword '(diminish 'subword-mode))
(eval-after-load 'simple '(diminish 'visual-line-mode))

(setq display-time-default-load-average nil)
(setq display-time-interval 1)
(setq display-time-format "%a, %b %d, %T ")
(display-time-mode 1)

(setq reb-re-syntax 'string) ;; the only sane option...

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Disable this per major mode or maybe using file size if it causes
;; performance issues?
(setq imenu-auto-rescan t)
(setq imenu-max-item-length 300)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(setq echo-keystrokes 0.25)

(setq initial-scratch-message "")

(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding
                                  ; (Chinese/Japanese/Korean
                                  ; characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

;; =============================================================================
;;                                                                   use-package
;; =============================================================================

(use-package server
  :config
  (progn
    (unless (server-running-p) (server-start))))

(use-package list-environment
  :ensure t)

(use-package paradox
  :config
  (progn
    (setq paradox-execute-asynchronously t)))

(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (sp-use-smartparens-bindings)
    (unbind-key "C-<backspace>" smartparens-mode-map)
    (unbind-key "M-<backspace>" smartparens-mode-map)
    (bind-key "C-)" 'sp-forward-slurp-sexp smartparens-mode-map)
    (bind-key "C-}" 'sp-forward-barf-sexp smartparens-mode-map)
    (bind-key "C-(" 'sp-backward-slurp-sexp smartparens-mode-map)
    (bind-key "C-{" 'sp-backward-barf-sexp smartparens-mode-map)))

(defclass indexed-mapping ()
  ((mapping :initarg :mapping :initform nil)
   (index :initarg :index :initform nil)))

(defmethod im-get ((im indexed-mapping) key)
  (plist-get (oref im :mapping) key))

(defmethod im-index-get ((im indexed-mapping) value)
  (plist-get (oref im :index) value))

(defmethod im-index-get-one ((im indexed-mapping) value)
  (let ((keys (plist-get (oref im :index) value)))
    (when keys (car keys))))

(defmethod im-put ((im indexed-mapping) key value)
  ;; Handle removing the key from where it is currently indexed
  (im-unindex im key value)
  ;; Add the key to its new position in the index
  (oset im :index
        (plist-put (oref im :index)
                   value (cons key (plist-get (oref im :index) value))))
  ;; Add the key, value pair to the mapping
  (oset im :mapping
        (plist-put (oref im :mapping) key value)))

(defmethod im-unindex ((im indexed-mapping) key value)
  (let* ((current-value (plist-get (oref im :mapping) key))
         (value-list (plist-get (oref im :index) current-value)))
    (when value-list
      (setq value-list (remove key value-list))
      (oset im :index
            (plist-put (oref im :index)
                       current-value value-list)))))

(defmethod im-delete ((im indexed-mapping) key)
  (im-unindex im key (im-get im key))
  (oset im :mapping
        (use-package-plist-delete (oref im :mapping) key)))

(use-package term
  :preface
  (progn
    (defvar imalison:buffer-index (make-instance indexed-mapping))

    (defun imalison:term-sym (dir)
      (let ((truename (file-truename dir)))
        (unless (string-equal (substring dir -1) "/")
          (setq truename (concat truename "/")))
        (intern truename)))

    (im-index-get-one imalison:buffer-index (imalison:term-sym default-directory))

    (defun imalison:build-term (directory)
      (let* ((default-directory directory)
             (program (getenv "SHELL"))
             (buffer (get-buffer (term-ansi-make-term directory program))))
        (im-put imalison:buffer-index buffer (imalison:term-sym directory))
        (with-current-buffer buffer
          (term-mode)
          (term-char-mode)
          (let (term-escape-char)
            (term-set-escape-char ?\C-x)))
        buffer))

    (defun imalison:term-buffer (directory)
      (let* ((directory-symbol (imalison:term-sym directory))
             (buffer (im-index-get-one imalison:buffer-index directory-symbol)))
        (while (and buffer (not (buffer-live-p buffer)))
          (im-delete imalison:buffer-index buffer)
          (setq buffer (im-index-get-one imalison:buffer-index directory-symbol)))
        (or buffer (imalison:build-term directory))))

    (defun imalison:projectile-term ()
      (interactive)
      (switch-to-buffer (imalison:term-buffer (projectile-project-root))))

    (defun imalison:dir-term ()
      (interactive)
      (switch-to-buffer (imalison:term-buffer default-directory)))

    (defun imalison:force-new-term ()
      (interactive)
      (switch-to-buffer (imalison:build-term default-directory)))

    (imalison:prefix-alternatives imalison:term imalison:projectile-term
                                  imalison:dir-term imalison:force-new-term))
  :config
  (progn
    (add-hook 'term-mode-hook 'imalison:disable-linum-mode)
    (advice-add 'term-handle-ansi-terminal-messages :after
                (lambda (&rest args)
                  (when (not (string-equal
                              (im-get imalison:buffer-index (current-buffer))
                              default-directory))
                    (im-put imalison:buffer-index (current-buffer)
                            (imalison:term-sym default-directory)))
                  (rename-buffer (format "term - %s" default-directory) t)))))

;; Set path from shell.
(use-package exec-path-from-shell
  :config
  (progn
    (add-to-list 'exec-path-from-shell-variables "GOPATH")
    (exec-path-from-shell-initialize)))

(use-package yasnippet
  :defer 5
  :commands (yas-global-mode)
  :config
  (progn
    (yas-global-mode)
    (diminish 'yas-minor-mode)
    (add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))
    (setq yas-prompt-functions
          (cons 'yas-ido-prompt
                (cl-delete 'yas-ido-prompt yas-prompt-functions)))))

(use-package tramp
  :commands tramp
  :config
  (setq tramp-default-method "scp"))

(use-package shackle
  :config
  (progn
    (diminish 'shackle-mode)
    (when nil                           ; disabled for now
      (shackle-mode))
    (setq shackle-inhibit-window-quit-on-same-windows t)
    (setq shackle-default-rule '(:same t))))

(use-package beacon
  :bind ("C-c b" . beacon-blink)
  :config
  (beacon-mode 1))

;; text mode stuff:
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq sentence-end-double-space nil)

;; y and n instead of yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package discover-my-major)

(use-package which-key
  :config
  (progn
    (setq which-key-idle-delay .50)
    (diminish 'which-key-mode)
    (which-key-mode)))

(use-package jump-char
  :bind (("C-;" . jump-char-forward)))

(imalison:prefix-alternatives imalison:avy avy-goto-word-1 avy-goto-char)
(use-package avy
  :bind (("C-j" . imalison:avy)
         ("M-g l" . avy-goto-line)
         ("C-'" . avy-goto-char-2)))

(imalison:prefix-alternatives imalison:ace-window ace-select-window ace-swap-window)
(use-package ace-window
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("C-c w" . imalison:ace-window))

(use-package flycheck
  :config
  (progn (global-flycheck-mode))
  :diminish flycheck-mode)

(use-package haskell-mode
  :commands haskell-mode
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)))

(use-package narrow-indirect
  :init
  (progn
    (define-key ctl-x-4-map "nd" 'ni-narrow-to-defun-indirect-other-window)
    (define-key ctl-x-4-map "nn" 'ni-narrow-to-region-indirect-other-window)
    (define-key ctl-x-4-map "np" 'ni-narrow-to-page-indirect-other-window)))

(use-package editorconfig
  :commands edconf-find-file-hook
  :demand t)

(use-package dtrt-indent
  :init (add-hook 'prog-mode-hook 'dtrt-indent-mode))

(use-package indent-guide
  :disabled t
  :config
  (progn
    (indent-guide-global-mode -1)
    (setq indent-guide-delay 0.1)))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (progn
    (add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode t)))))

(use-package diff-hl
  :config
  (progn (global-diff-hl-mode)))

(use-package gitolite-clone)

(use-package magit
  :commands magit-status
  :bind (("C-x g" . magit-status))
  :config
  (progn
    (defvar-setq magit-last-seen-setup-instructions "1.4.0")
    (setq magit-revert-buffers t)
    (use-package magit-filenotify
      ;; Seems like OSX does not support filenotify.
      :disabled t
      :if (emacs24_4-p)
      :config
      :init (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))))

(use-package company
  :commands company-mode imalison:company
  :bind (("C-\\" . imalison:company))
  :config
  (progn
    (imalison:prefix-alternatives
     imalison:company company-complete company-yasnippet)
    (setq company-idle-delay .25)
    (global-company-mode)
    (diminish 'company-mode))
  :init
  (add-hook 'prog-mode-hook (lambda () (company-mode t))))

(use-package expand-region
  :commands er/expand-region
  :config (setq expand-region-contract-fast-key "j")
  :bind (("C-c k" . er/expand-region)))

(use-package multiple-cursors
  :config
  (progn
    (use-package phi-search-mc
      :config
      (phi-search-mc/setup-keys))
    (use-package mc-extras
      :config
      (define-key mc/keymap (kbd "C-. =") 'mc/compare-chars)))
  :bind
   (("C-c m a" . mc/mark-all-like-this)
    ("C-c m m" . mc/mark-all-like-this-dwim)
    ("C-c m l" . mc/edit-lines)
    ("C-c m n" . mc/mark-next-like-this)
    ("C-c m p" . mc/mark-previous-like-this)
    ("C-c m s" . mc/mark-sgml-tag-pair)
    ("C-c m d" . mc/mark-all-like-this-in-defun)))

(use-package undo-tree
  :disabled t ;; this has been getting pretty annoying
  :bind (("C--" . undo-redo)
         ("C-c u" . undo-tree-visualize)
         ("C-c r" . undo-tree-redo))
  :config
  (diminish 'undo-tree-mode)
  :init
  (progn
    ;;(setq undo-tree-visualizer-diff t) ;; This causes performance problems
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)))

(use-package string-inflection
  :commands string-inflection-toggle
  :bind ("C-c l" . string-inflection-toggle))

(use-package load-dir
  :config
  (progn
    (add-to-list 'load-dirs "~/.emacs.d/load.d")
    (defvar site-lisp "/usr/share/emacs24/site-lisp/")
    (when (file-exists-p site-lisp) (add-to-list 'load-dirs site-lisp))))

(use-package recentf
  ;; binding is in helm.
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-menu-items 500)))

(use-package zop-to-char
  :bind ("M-z" . zop-to-char))

(use-package restclient
  :mode (("\\.restclient\\'" . restclient-mode))
  :config
  (progn
    (use-package company-restclient)))

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

(use-package emr
  :commands emr-initialize
  :init
  (progn
    (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
    (add-hook 'prog-mode-hook 'emr-initialize)))

(use-package git-link
  :config
  (progn
    (setq git-link-use-commit t)))

(use-package phabricator)

(use-package key-chord)

(use-package nodejs-repl)

;; =============================================================================
;;                                                         Non-Programming Stuff
;; =============================================================================

(use-package calc-mode
  :ensure nil
  :config
  (progn
    (setq calc-context-sensitive-enter t)))

(use-package helm-spotify
  :commands helm-spotify)

(use-package edit-server
  :commands edit-server-start
  :defer 1
  :config
  (progn
    (edit-server-start)
    (setq edit-server-new-frame nil)))

(use-package jabber
  :commands jabber-connect
  :config
  (progn
    (setq jabber-alert-presence-hooks nil)
    (defun jabber-message-content-message (from buffer text)
      (when (or jabber-message-alert-same-buffer
                (not (memq (selected-window) (get-buffer-window-list buffer))))
        (if (jabber-muc-sender-p from)
            (format "%s: %s" (jabber-jid-resource from) text)
          (format "%s: %s" (jabber-jid-displayname from) text))))
    (setq jabber-alert-message-function 'jabber-message-content-message)))

(use-package htmlize)

(use-package calfw
  :config
  (progn
    (require 'calfw-org)))

(use-package org
  :ensure org-plus-contrib
  :commands (org-mode org org-mobile-push org-mobile-pull org-agenda)
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c n t" . org-insert-todo-heading)
         ("C-c n s" . org-insert-todo-subheading)
         ("C-c n h" . org-insert-habit)
         ("C-c n m" . org-make-habit)
         ("C-c n l" . org-store-link)
         ("C-c n i" . org-insert-link)
         ("C-c C-t" . org-todo)
         ("C-c C-S-t" . org-todo-force-notes))
  :config
  (progn
    (setq org-global-properties
          '(quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                   ("STYLE_ALL" . "habit"))))
    (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
    (defvar-setq helm-org-headings-fontify t)
    (setq org-todo-repeat-to-state "TODO")

    (setq org-agenda-span 10)
    (setq org-agenda-start-day "-2d")

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((sh . t)
       (python . t)
       (ruby . t)
       (octave . t)
       (sqlite . t)))

    (when nil
      ;; Enable appointment notifications.
      (defadvice org-agenda-to-appt (before wickedcool activate)
        "Clear the appt-time-msg-list."
        (setq appt-time-msg-list nil))
      (appt-activate)
      (defun org-agenda-to-appt-no-message ()
        (suppress-messages (org-agenda-to-appt)))
      (run-at-time "00:00" 60 'org-agenda-to-appt-no-message))

    (defun org-archive-if (condition-function)
      (if (funcall condition-function)
          (let ((next-point-marker
                 (save-excursion (org-forward-heading-same-level 1) (point-marker))))
            (org-archive-subtree)
            (setq org-map-continue-from (marker-position next-point-marker)))))

    (defun org-archive-if-completed ()
      (interactive)
      (org-archive-if 'org-entry-is-done-p))

    (defun org-archive-completed-in-buffer ()
      (interactive)
      (org-map-entries 'org-archive-if-completed))

    (defun org-capture-make-todo-template (&optional content)
      (unless content (setq content "%?"))
      (with-temp-buffer
        (org-mode)
        (org-insert-heading)
        (insert content)
        (org-todo "TODO")
        (org-set-property "CREATED"
                          (with-temp-buffer
                            (org-insert-time-stamp
                             (org-current-effective-time) t t)))
        (remove-hook 'post-command-hook 'org-add-log-note)
        (org-add-log-note)
        (buffer-substring-no-properties (point-min) (point-max))))

    (defun org-todo-force-notes ()
      (interactive)
      (let ((org-todo-log-states
             (mapcar (lambda (state)
                       (list state 'note 'time))
                     (apply 'append org-todo-sets))))
        (cond ((eq major-mode 'org-mode)  (org-todo))
              ((eq major-mode 'org-agenda-mode) (org-agenda-todo)))))

    (defun org-make-habit ()
      (interactive)
      (org-set-property "STYLE" "habit"))

    (defun org-insert-habit ()
      (interactive)
      (org-insert-todo-heading nil)
      (org-make-habit))

    (defun org-todo-at-date (date)
      (interactive (list (org-time-string-to-time (org-read-date))))
      (flet ((org-current-effective-time (&rest r) date)
             (org-today (&rest r) (time-to-days date)))
        (cond ((eq major-mode 'org-mode) (org-todo))
              ((eq major-mode 'org-agenda-mode) (org-agenda-todo)))))

    (defun org-capture-make-linked-todo-template ()
      (org-capture-make-todo-template "%? %A"))

    (defun org-cmp-creation-times (a b)
      (let ((a-created (get-date-created-from-agenda-entry a))
            (b-created (get-date-created-from-agenda-entry b)))
        (imalison:compare-int-list a-created b-created)))

    (defun org-agenda-done (&optional arg)
      "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
      (interactive "P")
      (org-agenda-todo "DONE"))
    ;; Override the key definition for org-exit
    ;; (define-key org-agenda-mode-map "x" #'org-agenda-done) ;; TODO why does this cause an error

    ;; org-mode add-ons
    (use-package org-present :ensure t)
    (use-package org-pomodoro :ensure t)

    ;; variable configuration
    (add-to-list 'org-modules 'org-habit)
    (add-to-list 'org-modules 'org-expiry)
    (add-to-list 'org-modules 'org-notify)

    (setq org-src-fontify-natively t)
    (setq org-habit-graph-column 50)
    (setq org-habit-show-habits-only-for-today t)
    ;; My priority system:

    ;; A - Absolutely MUST, at all costs, be completed by the provided
    ;;     due date. TODO: implement some type of extreme nagging
    ;;     system that alerts in an intrusive way for overdue A
    ;;     priority tasks.

    ;; B - Should be given immediate attention if the due date is any
    ;;     time in the next two days. Failure to meet due date would
    ;;     be bad but not catastrophic.

    ;; C - The highest priority to which tasks for which failure to
    ;;     complete on time would not have considerable significant
    ;;     consequences. There is still significant reason to prefer
    ;;     the completion of these tasks sooner rather than later.

    ;; D - Failure to complete within a few days (or ever) of any
    ;;     deadline would be completely okay. As such, any deadline
    ;;     present on such a task is necessarily self imposed. Still
    ;;     probably worth doing

    ;; E - Potentially not even worth doing at all, but worth taking a
    ;;     note about in case it comes up again, or becomes more
    ;;     interesting later.

    ;; F - Almost certainly not worth attempting in the immediate future.
    ;;     Just brain dump.

    ;; Priorities are somewhat contextual within each category. Things
    ;; in the gtd or work categories are generally regarded as much
    ;; more important than things with the same priority from the
    ;; dotfiles category.

    ;; Items without deadlines or scheduled times of a given priority
    ;; can be regarded as less important than items that DO have
    ;; deadlines of that same priority.

    (setq org-lowest-priority 69) ;; The character E
    (setq org-completion-use-ido t)
    (setq org-enforce-todo-dependencies t)
    (setq org-deadline-warning-days 0)
    (setq org-default-priority ?D)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)
    ;;(add-to-list org-agenda-tag-filter-preset "+PRIORITY<\"C\"")

    (use-package org-notify
      :disabled t
      :config
      (progn
        (defun imalison:org-notify-notification-handler (plist)
          (sauron-add-event 'org-notify 4 (format "%s, %s.\n" (plist-get plist :heading)
                                                  (org-notify-body-text plist))))

        (setq org-show-notification-handler 'imalison:org-notify-notification-handler)

        (org-notify-add 'default '(:time "1h" :actions imalison:org-notify-notification-handler
        				 :period "2m" :duration 60))
        (org-notify-add 'default '(:time "100m" :actions imalison:org-notify-notification-handler
                                         :period "2m" :duration 60))
        (org-notify-add 'urgent-second '(:time "3m" :actions (-notify/window -ding)
                                               :period "15s" :duration 10))
        (org-notify-add 'minute '(:time "5m" :actions -notify/window
                                        :period "100s" :duration 70))
        (org-notify-add '12hours
                        '(:time "3m" :actions (-notify/window -ding)
                                :period "15s" :duration 10)
                        '(:time "100m" :actions -notify/window
                                :period "2m" :duration 60)
                        '(:time "12h" :actions -notify/window :audible nil
                                :period "10m" :duration 200))
        (org-notify-add '5days
                        '(:time "100m" :actions -notify/window
                                :period "2m" :duration 60)
                        '(:time "2d" :actions -notify/window
                                :period "15m" :duration 100)
                        '(:time "5d" :actions -notify/window
                                :period "2h" :duration 200))
        (org-notify-add 'long-20days
                        '(:time "2d" :actions -notify/window
                                :period "15m" :duration 60)
                        '(:time "5d" :actions -notify/window
                                :period "2h" :duration 60)
                        '(:time "20d" :actions -email :period "2d" :audible nil))
        (org-notify-add 'long-50days
                        '(:time "4d" :actions -notify/window
                                :period "30m" :duration 100)
                        '(:time "10d" :actions -notify/window
                                :period "4h" :duration 200)
                        '(:time "50d" :actions -email :period "3d" :audible nil))
        (org-notify-add 'long-100days
                        '(:time "2d" :actions -notify/window
                                :period "1h" :duration 200)
                        '(:time "10d" :actions -notify/window
                                :period "10h" :duration 300)
                        '(:time "50d" :actions -email :period "3d" :audible nil)
                        '(:time "100d" :actions -email :period "5d" :audible nil))
        (org-notify-start 10)))

    (use-package org-bullets
      :config
      (progn
        (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

    (use-package org-ehtml
      :disabled t
      :config
      (progn
        (setq org-ehtml-docroot (expand-file-name "~/Dropbox/org"))
        (setq org-ehtml-allow-agenda t)
        (setq org-ehtml-editable-headlines t)
        (setq org-ehtml-everything-editable t)))

    ;; Agenda setup.
    (defvar-if-non-existent imalison:org-gtd-file "~/org/gtd.org")
    (defvar-if-non-existent imalison:org-habits-file "~/org/habits.org")
    (defvar-if-non-existent imalison:org-calendar-file "~/org/calendar.org")

    (unless (boundp 'org-capture-templates)
      (defvar org-capture-templates nil))

    (defun imalison:add-to-org-agenda-files (incoming-files)
      (setq org-agenda-files (delete-dups
       (cl-loop for filepath in (append org-agenda-files incoming-files)
                when (and filepath (file-exists-p (file-truename filepath)))
                collect (file-truename filepath)))))

    (imalison:add-to-org-agenda-files
     (list imalison:org-gtd-file imalison:org-habits-file
           imalison:org-calendar-file))

    (add-to-list 'org-capture-templates
                 `("t" "GTD Todo (Linked)" entry (file ,imalison:org-gtd-file)
                   (function org-capture-make-linked-todo-template)))

    (add-to-list 'org-capture-templates
                 `("g" "GTD Todo" entry (file ,imalison:org-gtd-file)
                   (function org-capture-make-todo-template)))

    (add-to-list 'org-capture-templates
                 `("y" "Calendar entry (Linked)" entry
                   (file ,imalison:org-calendar-file)
                   "* %? %A
  :PROPERTIES:
  :CREATED: %U
  :END:
%^T"))

    (add-to-list 'org-capture-templates
                 `("c" "Calendar entry" entry
                   (file ,imalison:org-calendar-file)
                   "* %?
  :PROPERTIES:
  :CREATED: %U
  :END:
%^T"))

    (add-to-list 'org-capture-templates
                 `("h" "Habit" entry (file ,imalison:org-habits-file)
                   "* TODO
  SCHEDULED: %^t
  :PROPERTIES:
  :CREATED: %U
  :STYLE: habit
  :END:"))

    (let ((this-week-high-priority
           ;; The < in the following line works has behavior that is opposite
           ;; to what one might expect.
           '(tags-todo "+PRIORITY<\"C\"+DEADLINE<\"<+1w>\"DEADLINE>\"<+0d>\""
                       ((org-agenda-overriding-header
                         "Upcoming high priority tasks:"))))
          (due-today '(tags-todo
                           "+DEADLINE=<\"<+0d>\""
                           ((org-agenda-overriding-header
                             "Due today:"))))
          (recently-created '(tags-todo
                           "+CREATED=>\"<-3d>\""
                           ((org-agenda-overriding-header "Recently created:")
                            (org-agenda-cmp-user-defined 'org-cmp-creation-times)
                            (org-agenda-sorting-strategy '(user-defined-down)))))
          (next '(todo "NEXT"))
          (started '(todo "STARTED"))
          (missing-deadline
           '(tags-todo "-DEADLINE={.}/!"
                       ((org-agenda-overriding-header
                         "These don't have deadlines:"))))
          (missing-priority
           '(tags-todo "-PRIORITY={.}/!"
                       ((org-agenda-overriding-header
                         "These don't have priorities:")))))

      (setq org-agenda-custom-commands
            `(("M" "Main agenda view"
               ((agenda ""
                        ((org-agenda-overriding-header "Agenda:")
                         (org-agenda-ndays 5)
                         (org-deadline-warning-days 0)))
                ,due-today
                ,next
                ,started
                ,this-week-high-priority
                ,recently-created)
               nil nil)
              ,(cons "A" (cons "High priority upcoming" this-week-high-priority))
              ,(cons "d" (cons "Overdue tasks and due today" due-today))
              ,(cons "r" (cons "Recently created" recently-created))
	      ("h" "A, B priority:" tags-todo "+PRIORITY<\"C\""
                       ((org-agenda-overriding-header
                         "High Priority:")))
              ("c" "At least priority C:" tags-todo "+PRIORITY<\"D\""
                       ((org-agenda-overriding-header
                         "At least priority C:"))))))

    ;; What follows is a description of the significance of each of
    ;; the values available in `org-todo-keywords'. All headings with
    ;; one of these keywords deal with the concept of the completion
    ;; of some task or collection of tasks to bring about a particular
    ;; state of affairs. In some cases, the actual tasks involved may
    ;; not be known at the time of task creation.

    ;; Uncompleted States:

    ;; IDEA - This TODO exists in only the most abstract sense: it is
    ;; an imagined state of affairs that requires tasks that are
    ;; either not yet known, or have not thoroughly been considered.

    ;; RESEARCH - This TODO needs to be investigated further before
    ;; action can be taken to achieve the desired outcome. It is not
    ;; known how much time and effort will be consumed in the actual
    ;; completion of the task.

    ;; TODO - The scope and work involved in this TODO are well
    ;; understood, but for some reason or another, it is not something
    ;; that should be attempted in the immediate future. Typically
    ;; this is because the task is not considered a top priority, but
    ;; it may also be for some other reason.

    ;; NEXT - This TODO is immediately actionable and should be
    ;; started in the immediate future.

    ;; STARTED - Work on this TODO has already started, further work
    ;; is immediately actionable.

    ;; WAIT - The work involved in this TODO is well understood, but
    ;; it is blocked for the time being.

    ;; BACKLOG - While technically actionable, this task is not only
    ;; in the immediate future, but the foreseable future. It exists
    ;; as a task mostly as a note/reminder in case it becomes higher
    ;; priority in the future.

    ;; Completed States:

    ;; DONE - This TODO has been completed exactly as imagined.

    ;; HANDLED - This TODO was completed in spirit, though not by the
    ;; means that were originally imagined/outlined in the TODO.

    ;; EXPIRED - The owner of this TODO failed to take action on it
    ;; within the appropriate time period, and there is now no point in
    ;; attempting it.

    ;; CANCELED - For whatever reason, this TODO should no longer be
    ;; attempted. This TODO is typically used in contrast to the
    ;; EXPIRED TODO to indicate that the owner is not necessarily to
    ;; blame.

    (setq org-todo-keywords
          '((sequence "IDEA(i!)" "RESEARCH(r!)" "TODO(t!)" "NEXT(n!)" "STARTED(s!)" "WAIT(w!)" "BACKLOG(b!)" "|"
                      "DONE(d!)" "HANDLED(h!)" "EXPIRED(e!)" "CANCELED(c!)")
            (sequence "BASKET(!)" "CLEAN(!)" "DRY(!)" "|" "FOLDED(!)")))

    ;; Record changes to todo states
    (setq org-log-into-drawer t)
    ;; Stop starting agenda from deleting frame setup!
    (setq org-agenda-window-setup 'other-window)
    (define-key mode-specific-map [?a] 'org-agenda)
    (unbind-key "C-j" org-mode-map))
  :init
  (progn
    (setq org-directory "~/Dropbox/org")
    (setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
    (add-hook 'org-mode-hook 'imalison:disable-linum-mode)
    (add-hook 'org-mode-hook 'imalison:disable-smartparens-mode)
    (add-hook 'org-mode-hook (lambda () (setq org-todo-key-trigger t)))
    (add-hook 'org-agenda-mode-hook 'imalison:disable-linum-mode)))

(use-package clocker :ensure t)

(use-package org-projectile
  :demand t
  :bind (("C-c n p" . imalison:helm-org-todo))
  :config
  (progn
    (org-projectile:prompt)
    (add-to-list 'org-capture-templates
                 (org-projectile:project-todo-entry
                  "l" "* TODO %? %a\n" "Linked Project TODO"))
    (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p"))
    (setq org-confirm-elisp-link-function nil)
    (imalison:add-to-org-agenda-files (org-projectile:todo-files))
    (defun imalison:helm-org-todo (&optional arg)
      (interactive "P")
      (helm :sources (list (helm-source-org-capture-templates)
                           (org-projectile:helm-source
                            (if arg (org-capture-make-linked-todo-template)
                              (org-capture-make-todo-template))))
            :candidate-number-limit 99999
            :buffer "*helm org capture templates*"))))

(use-package deft
  :config
  (progn
    (setq deft-default-extension "org")
    (setq deft-extensions '("org"))
    (setq deft-use-filter-string-for-filename t)
    (setq deft-file-naming-rules '((noslash . "_")
                                   (nospace . "_")
                                   (case-fn . downcase)))
    (setq deft-directory "~/SparkleShare/org/notes")))

(use-package epg
  :config
  (epa-file-enable))

(use-package twittering-mode
  :commands twittering-mode)

(use-package matrix-client)

(use-package erc
  :commands erc
  :config
  (progn
    ;; (add-to-list 'erc-modules 'notifications)
    ;; logging:
    (require 'erc-log)
    (setq erc-log-channels-directory "~/Dropbox (Personal)/irclogs")
    (erc-log-enable)
    (use-package erc-colorize :ensure t) (erc-colorize-mode 1)))

(use-package bitlbee
  :config
  (progn
    (defvar bitlbee-password "geheim")
    (add-hook 'erc-join-hook 'bitlbee-identify)
    (defun bitlbee-identify ()
      "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
      (when (and (string= "localhost" erc-session-server)
                 (string= "&bitlbee" (buffer-name)))
        (erc-message "PRIVMSG" (format "%s identify %s"
                                       (erc-default-target)
                                       bitlbee-password))))))

(use-package s :ensure t)
(add-to-list 'load-path (s-trim (shell-command-to-string "mu4e_directory")))

(use-package mu4e
  :ensure nil
  :commands (mu4e mu4e-view-message-with-msgid mu4e-update-index email)
  :bind ("C-c 0" . email)
  :config
  (progn
    (defun email (&optional arg)
      (interactive "P")
      (if (string-equal (persp-name persp-curr) "email")
          (progn (delete-other-windows) (mu4e))
        (progn
          (persp-switch "email")
          (when (or (not (mu4e-running-p)) arg)
            (delete-other-windows) (mu4e)))))
    ;; enable inline images
    (setq mu4e-view-show-images t)
    ;; show images
    (setq mu4e-show-images t)
    ;; Try to display html as text
    (setq mu4e-view-prefer-html nil)

    (setq mu4e-html2text-command "html2text -width 80 -nobs -utf8")

    ;; use imagemagick, if available
    (when (fboundp 'imagemagick-register-types)
         (imagemagick-register-types))
    (setq mail-user-agent 'mu4e-user-agent)
    (require 'org-mu4e)
    (setq mu4e-compose-complete-only-after nil)
    (setq mu4e-maildir "~/Mail")

    (setq mu4e-drafts-folder "/[Gmail].Drafts")
    (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
    (setq mu4e-trash-folder  "/[Gmail].Trash")

    (setq mu4e-sent-messages-behavior 'delete)
    (setq mu4e-headers-skip-duplicates t)
    (setq mu4e-update-interval (* 60 20))
    (setq message-kill-buffer-on-exit t)
    (setq mail-user-agent 'mu4e-user-agent) ;; make mu4e the default mail client

    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
    (setq mu4e-sent-messages-behavior 'delete)

    ;; allow for updating mail using 'U' in the main view:
    (setq mu4e-get-mail-command "timeout 60 offlineimap")

    (add-hook 'mu4e-compose-mode-hook
              (defun my-do-compose-stuff () (flyspell-mode)))

    (add-to-list 'mu4e-headers-actions '("view in browser" . mu4e-action-view-in-browser))
    (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser))

    (defun mu4e-view (msg headersbuf)
      "Display the message MSG in a new buffer, and keep in sync with HDRSBUF.
'In sync' here means that moving to the next/previous message in
the the message view affects HDRSBUF, as does marking etc.

As a side-effect, a message that is being viewed loses its 'unread'
marking if it still had that."
      (let* ((embedded ;; is it as an embedded msg (ie. message/rfc822 att)?
              (when (gethash (mu4e-message-field msg :path)
                             mu4e~path-parent-docid-map) t))
             (buf
              (if embedded
                  (mu4e~view-embedded-winbuf)
                (get-buffer-create mu4e~view-buffer-name))))
        ;; note: mu4e~view-mark-as-read will pseudo-recursively call mu4e-view again
        ;; by triggering mu4e~view again as it marks the message as read
        (with-current-buffer buf
          (switch-to-buffer buf)
          (setq mu4e~view-msg msg)
          (when (or (mu4e~view-mark-as-read msg) t) ;;(or embedded (not (mu4e~view-mark-as-read msg)))
            (let ((inhibit-read-only t))
              (erase-buffer)
              (mu4e~delete-all-overlays)
              (insert (mu4e-view-message-text msg))
              (goto-char (point-min))
              (mu4e~fontify-cited)
              (mu4e~fontify-signature)
              (mu4e~view-make-urls-clickable)
              (mu4e~view-show-images-maybe msg)
              (setq
               mu4e~view-buffer buf
               mu4e~view-headers-buffer headersbuf)
              (when embedded (local-set-key "q" 'kill-buffer-and-window))
              (mu4e-view-mode))))))

    (require 'smtpmail)

    ;; alternatively, for emacs-24 you can use:
    (setq message-send-mail-function 'smtpmail-send-it
          smtpmail-stream-type 'starttls
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587)))

(use-package gmail-message-mode :ensure t)

(use-package alert
  :config
  (progn
    (defun alert-notifier-notify (info)
      (if alert-notifier-command
          (let ((args
                 (list "-title"   (alert-encode-string (plist-get info :title))
                       "-activate" "org.gnu.Emacs"
                       "-message" (alert-encode-string (plist-get info :message))
                       "-execute" (format "\"%s\"" (switch-to-buffer-command (plist-get info :buffer))))))
            (apply #'call-process alert-notifier-command nil nil nil args))
        (alert-message-notify info)))

    (defun switch-to-buffer-command (buffer-name)
      (emacsclient-command (format "(switch-to-buffer \\\"%s\\\")" buffer-name)))

    (defun emacsclient-command (command)
      (format "emacsclient --server-file='%s' -e '%s'" server-name command))

    (setq alert-default-style 'notifier)))

(use-package sauron
  :defer 5
  :commands (sauron-start sauron-start-hidden)
  :init
  (progn
    (when (eq system-type 'darwin)
      (setq sauron-modules '(sauron-erc sauron-org sauron-notifications
                                        sauron-twittering sauron-jabber sauron-identica))
      (defun sauron-dbus-start ()
        nil)
      (makunbound 'dbus-path-emacs)))
  :config
  (progn
    (sauron-start-hidden)
    ;; This should really check (featurep 'dbus) but for some reason
    ;; this is always true even if support is not there.
    (setq sauron-prio-sauron-started 2)
    (setq sauron-min-priority 3)
    ;; (setq sauron-dbus-cookie t) ;; linux only?
    (setq sauron-separate-frame nil)
    (setq sauron-nick-insensitivity 1)
    (defun sauron:jabber-notify (origin priority message &optional properties)
      (funcall notify-function "gtalk" message))
    (defun sauron:erc-notify (origin priority message &optional properties)
      (let ((event (plist-get properties :event)))
        (funcall notify-function "IRC" message)))
    (defun sauron:mu4e-notify (origin priority message &optional properties)
      nil)
    (defun sauron:dbus-notify (origin priority message &optional properties)
      (funcall notify-function "GMail" message))
    (defun sauron:dispatch-notify (origin priority message &optional properties)
      (let ((handler (cond ((string= origin "erc") 'sauron:erc-notify)
                            ((string= origin "jabber") 'sauron:jabber-notify)
                            ((string= origin "mu4e") 'sauron:mu4e-notify)
                            ((string= origin "dbus") 'sauron:dbus-notify)
                            (t (lambda (&rest r) nil)))))
        (funcall handler origin priority message properties)))
    ;; Prefering alert.el for now ;; (add-hook 'sauron-event-added-functions 'sauron:dispatch-notify)
    (sauron-start-hidden)
    (add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter)))

(use-package screenshot)

(use-package floobits)

(use-package wsd-mode)

(use-package libmpdee)

(use-package flyspell
  :disabled t                           ; kind of annoying
  :preface (setq flyspell-issue-welcome-flag nil)
  :config
  (progn
    (diminish 'flyspell-mode)
    (bind-key "M-s" 'flyspell-correct-word-before-point flyspell-mode-map)
    (unbind-key "C-;" flyspell-mode-map)
    (defun flyspell-emacs-popup-textual (event poss word)
      "A textual flyspell popup menu."
      (let* ((corrects (if flyspell-sort-corrections
                           (sort (car (cdr (cdr poss))) 'string<)
                         (car (cdr (cdr poss)))))
             (cor-menu (if (consp corrects)
                           (mapcar (lambda (correct)
                                     (list correct correct))
                                   corrects)
                         '()))
             (affix (car (cdr (cdr (cdr poss)))))
             show-affix-info
             (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                         (list
                                          (list (concat "Save affix: "
                                                        (car affix))
                                                'save)
                                          '("Accept (session)" session)
                                          '("Accept (buffer)" buffer))
                                       '(("Save word" save)
                                         ("Accept (session)" session)
                                         ("Accept (buffer)" buffer)))))
                           (if (consp cor-menu)
                               (append cor-menu (cons "" save))
                             save)))
             (menu (mapcar
                    (lambda (arg) (if (consp arg) (car arg) arg))
                    base-menu)))
        (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))
    (fset 'flyspell-emacs-popup 'flyspell-emacs-popup-textual)))

;; =============================================================================
;;                                                        Programming Mode Hooks
;; =============================================================================

(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))
(add-hook 'prog-mode-hook (lambda () (subword-mode t) (diminish 'subword-mode)))
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; (add-hook 'prog-mode-hook (lambda () (highlight-lines-matching-regexp
;;                                  ".\\{81\\}" 'hi-blue)))

;; =============================================================================
;;                                          File Navigation: helm/projectile/ido
;; =============================================================================

(use-package web-mode
  :mode (("\\.tmpl\\'" . web-mode)
         ("\\.cql\\'" . web-mode))
  :config
  (progn
    (defvar-setq web-mode-content-types-alist
      '(("gtl" . "\\.tmpl\\'")
        ("gtl" . "\\.cql\\'")))))

(use-package helm-themes)

(use-package helm-config
  :ensure helm
  :demand t
  :bind (("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x C-i" . helm-imenu)
         ("C-h a" . helm-apropos)
         ("C-c C-h" . helm-org-agenda-files-headings)
         ("C-c ;" . helm-recentf))
  :init
  (progn
    (helm-mode 1)
    (use-package helm-ag
      :bind ("C-c p S" . imalison:set-helm-ag-extra-options)
      :config
      (defun imalison:set-helm-ag-extra-options ()
        (interactive)
        (let ((option (read-string "Extra options: " (or helm-ag--extra-options "")
                                   'helm-ag--extra-options-history)))
          (setq helm-ag--extra-options option)))))
  :config
  (progn
    (setq helm-split-window-default-side 'same)
    (defun helm-source-org-capture-templates ()
      (helm-build-sync-source "Org Capture Templates:"
        :candidates (cl-loop for template in org-capture-templates
                             collect `(,(nth 1 template) . ,(nth 0 template)))
        :action '(("Do capture" . (lambda (template-shortcut)
                                    (org-capture nil template-shortcut))))))
    (defun helm-org-capture-templates ()
      (interactive)
      (helm :sources (helm-source-org-capture-templates)
            :candidate-number-limit 99999
            :buffer "*helm org capture templates*"))
    (cl-defun helm-org-headings-in-buffer ()
      (interactive)
      (helm :sources (helm-source-org-headings-for-files
                      (list (projectile-completing-read
                             "File to look at headings from: "
                             (projectile-all-project-files))))
            :candidate-number-limit 99999
            :buffer "*helm org inbuffer*"))
    ;; helm zsh source history
    (defvar helm-c-source-zsh-history
      '((name . "Zsh History")
        (candidates . helm-c-zsh-history-set-candidates)
        (action . (("Execute Command" . helm-c-zsh-history-action)))
        (volatile)
        (requires-pattern . 3)
        (delayed)))
    (defun helm-c-zsh-history-set-candidates (&optional request-prefix)
      (let ((pattern (replace-regexp-in-string
                      " " ".*"
                      (or (and request-prefix
                               (concat request-prefix
                                       " " helm-pattern))
                          helm-pattern))))
        (with-current-buffer (find-file-noselect "~/.zsh_history" t t)
          (auto-revert-mode -1)
          (goto-char (point-max))
          (loop for pos = (re-search-backward pattern nil t)
                while pos
                collect (replace-regexp-in-string
                         "\\`:.+?;" ""
                         (buffer-substring (line-beginning-position)
                                           (line-end-position)))))))

    (defun helm-c-zsh-history-action (candidate)
      (async-shell-command candidate))

    (defun helm-command-from-zsh ()
      (interactive)
      (require 'helm)
      (helm-other-buffer 'helm-c-source-zsh-history "*helm zsh history*"))

    (use-package helm-descbinds
      :demand t
      :config (helm-descbinds-mode 1))
    (helm-mode 1)
    (diminish 'helm-mode)))

(use-package helm-swoop
  :bind ("C-S-s" . helm-swoop)
  :commands helm-swoop)

(use-package perspective
  :demand t
  :config
  (progn
    (persp-mode)
    (defun persp-get-perspectives-for-buffer (buffer)
      "Get the names of all of the perspectives of which `buffer` is a member."
      (cl-loop for perspective being the hash-value of perspectives-hash
               if (member buffer (persp-buffers perspective))
               collect (persp-name perspective)))

    (defun persp-pick-perspective-by-buffer (buffer)
  "Select a buffer and go to the perspective to which that buffer
belongs. If the buffer belongs to more than one perspective
completion will be used to pick the perspective to switch to.
Switch the focus to the window in which said buffer is displayed
if such a window exists. Otherwise display the buffer in whatever
window is active in the perspective."
  (interactive (list (funcall persp-interactive-completion-function
                              "Buffer: " (mapcar 'buffer-name (buffer-list)))))
  (let* ((perspectives (persp-get-perspectives-for-buffer (get-buffer buffer)))
         (perspective (if (> (length perspectives) 1)
                          (funcall persp-interactive-completion-function
                                   (format "Select the perspective in which you would like to visit %s."
                                           buffer)
                                   perspectives)
                                   (car perspectives))))
    (if (string= (persp-name persp-curr) perspective)
        ;; This allows the opening of a single buffer in more than one window
        ;; in a single perspective.
        (switch-to-buffer buffer)
      (progn
          (persp-switch perspective)
          (if (get-buffer-window buffer)
              (set-frame-selected-window nil (get-buffer-window buffer))
            (switch-to-buffer buffer))))))

    (defun persp-mode-switch-buffers (arg)
      (interactive "P")
      (if arg (call-interactively 'ido-switch-buffer)
        (call-interactively 'persp-pick-perspective-by-buffer)))

    (define-key persp-mode-map (kbd "C-x b") 'persp-mode-switch-buffers))
  :bind ("C-c 9" . persp-switch))

(use-package helm-projectile
  :commands (helm-projectile-on)
  :config
  (progn
    (helm-delete-action-from-source "Search in Project" helm-source-projectile-projects)
    (helm-delete-action-from-source "Open term for project" helm-source-projectile-projects)
    (defun imalison:invalidate-cache-and-open-file (dir)
      (projectile-invalidate-cache nil)
      (projectile-find-file))
    (defun imalison:switch-to-project-and-search (dir)
      (let ((default-directory dir)
            (projectile-require-project-root nil)
            (helm-action-buffer "this-buffer-should-not-exist"))
        (helm-projectile-ag)))
    (helm-add-action-to-source "Search in Project"
                               'imalison:switch-to-project-and-search
                               helm-source-projectile-projects)
    (helm-add-action-to-source "Invalidate Cache and Open File"
                               'imalison:invalidate-cache-and-open-file
                               helm-source-projectile-projects)
    (helm-add-action-to-source "Open term for project"
                               (lambda (dir)
                                 (let ((default-directory dir)
                                       (projectile-require-project-root nil)
                                       (helm-action-buffer "this-buffer-should-not-exist"))
                                   (imalison:term nil)))
                                 helm-source-projectile-projects)))

(use-package projectile
  :demand t
  :config
  (progn
    (defun projectile-make-all-subdirs-projects (directory)
      (cl-loop for file-info in (directory-files-and-attributes directory)
               do (when (nth 1 file-info)
                    (write-region "" nil
                                  (expand-file-name
                                   (concat directory "/"
                                           (nth 0 file-info) "/.projectile"))))))
    (setq helm-ag-always-set-extra-option nil)
    (defun imalison:do-ag (&optional arg)
      (interactive "P")
      (if arg (helm-do-ag) (helm-projectile-ag)))
    (projectile-global-mode)
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'helm)
    (add-to-list 'projectile-globally-ignored-files "Godeps")
    (add-to-list 'projectile-globally-ignored-files "thrift-binaries")
    (add-to-list 'projectile-globally-ignored-files "go-build")
    (helm-projectile-on)
    (diminish 'projectile-mode)
    (unbind-key "C-c p S" projectile-command-map)
    (unbind-key "C-c p s a" projectile-command-map)
    (unbind-key "C-c p s g" projectile-command-map)
    (unbind-key "C-c p s s" projectile-command-map)
    (unbind-key "C-c p s" projectile-command-map)
    (unbind-key "C-c p f" projectile-command-map)
    (bind-key* "C-c p s" 'imalison:do-ag)
    (bind-key* "C-c p f" 'imalison:projectile-find-file)
    (defun imalison:projectile-find-file (arg)
          (interactive "P")
          (if arg
              (projectile-find-file-other-window)
              (projectile-find-file))))
  :bind (("C-x f" . projectile-find-file-in-known-projects)
         ("C-c p f" . imalison:projectile-find-file))
  :init
  (progn
    (use-package persp-projectile
      :commands projectile-persp-switch-project)))

(use-package smex
  ;; Using helm-M-x instead
  :disabled t
  :commands smex
  ;; This is here because smex feels like part of ido
  :bind ("M-x" . smex))

(use-package ido
  :commands ido-mode
  :config
  (progn
    (setq ido-auto-merge-work-directories-length -1)
    (setq ido-use-filename-at-point nil)
    (setq ido-create-new-buffer 'always)
    (ido-everywhere 1)
    (setq ido-enable-flex-matching t)
    (use-package flx)
    (use-package flx-ido
      :commands flx-ido-mode
      :init (flx-ido-mode 1)
      :config
      (progn
        ;; disable ido faces to see flx highlights.
        ;; This makes flx-ido much faster.
        (setq gc-cons-threshold 20000000)
        (setq ido-use-faces nil)))
    (use-package ido-ubiquitous
      :disabled t
      :commands (ido-ubiquitous-mode))
    (use-package ido-vertical-mode
      :config
      (progn
        (ido-vertical-mode 1)
        (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)))
    (use-package flx-ido)))


(when (or (and (boundp 'use-ido) use-ido) (not (boundp 'use-ido))) (ido-mode 1))

;; =============================================================================
;;                                                                         elisp
;; =============================================================================

(setq edebug-trace t)

(use-package macrostep)

(use-package elisp-slime-nav
  :commands elisp-slime-nav-mode
  :config
  (diminish 'elisp-slime-nav-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t))))


(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (setq imenu-space-replacement nil)
  (add-to-list 'imenu-generic-expression
               `("Package"
                 ,"(use-package \\(.+\\)$" 1))
  (add-to-list 'imenu-generic-expression
               `("Section"
                 ,(concat ";\\{1,4\\} =\\{10,80\\}\n;\\{1,4\\} \\{10,80\\}"
                          "\\(.+\\)$") 1) t))

(put 'use-package 'lisp-indent-function 1) ;; reduce indentation for use-package
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
(add-hook 'emacs-lisp-mode-hook 'flatten-current-imenu-index-function)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq indent-tabs-mode nil)
                                  (setq show-trailing-whitespace t)))
(define-key lisp-mode-shared-map (kbd "C-c C-c") 'eval-defun)
(define-key lisp-mode-shared-map (kbd "C-c C-r") 'eval-and-replace)
(define-key lisp-mode-shared-map (kbd "C-c o r") 'up-list-region)
(define-key lisp-mode-shared-map (kbd "C-c o o") 'up-list-back)
(define-key lisp-mode-shared-map (kbd "C-x C-e") 'eval-region-or-last-sexp)
(unbind-key "C-j" lisp-interaction-mode-map)

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

(defun imalison:project-root-or-current-directory ()
  (if (projectile-project-p)
      (projectile-project-root) (if (buffer-file-name) (file-name-directory (buffer-file-name)))))

(defun get-virtual-envs ()
  (let ((project-root (imalison:project-root-or-current-directory)))
    (when project-root
      (condition-case ex
          (cl-remove-if-not 'file-exists-p
                            (mapcar (lambda (env-suffix)
                                      (concat project-root env-suffix))
                                    '(".tox/py27/" "env/" ".tox/venv/")))
        ('error
         (message (format "Caught exception: [%s]" ex))
         (setq retval (cons 'exception (list ex))))
        nil))))

(defun message-virtual-envs ()
  (interactive)
  (message "%s" (get-virtual-envs)))

(use-package jedi
  :commands (jedi:goto-definition jedi-mode)
  :config
  (progn
    (setq jedi:complete-on-dot t)
    (setq jedi:imenu-create-index-function 'jedi:create-flat-imenu-index))
  :bind (("M-." . jedi:goto-definition)
         ("M-," . jedi:goto-definition-pop-marker)))

(use-package company-jedi
  :commands company-jedi
  :ensure t)

(use-package python
  :commands python-mode
  :mode ("\\.py\\'" . python-mode)
  :config
  (progn
    (fset 'main "if __name__ == '__main__':")
    (fset 'sphinx-class ":class:`~")
  :init
  (progn
    (use-package pymacs :ensure t)
    (use-package sphinx-doc :ensure t)
    (defun imalison:python-mode ()
      (setq show-trailing-whitespace t)
      (if use-python-tabs (python-tabs))
      (subword-mode t)
      (imalison:make-imenu-index-flat)
      (jedi:setup)
      (add-virtual-envs-to-jedi-server)
      (remove-hook 'completion-at-point-functions
                   'python-completion-complete-at-point 'local)
      (set (make-local-variable 'company-backends) '(company-jedi)))
    (add-hook 'python-mode-hook #'imalison:python-mode))))

;; =============================================================================
;;                                                                         Scala
;; =============================================================================

(use-package scala-mode2
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sc\\'" . scala-mode))
  :config
  (progn
    (when (use-package ensime
            :commands ensime-mode
            :config
            (bind-key "M-," 'ensime-pop-find-definition-stack ensime-mode-map))
      (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))
    (setq scala-indent:align-parameters t)))

;; =============================================================================
;;                                                                          Java
;; =============================================================================

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode t)))

(use-package android-mode
  :config
  (progn
    (setq android-mode-sdk-dir
          (s-trim (shell-command-to-string "android_sdk_directory")))))

(use-package gradle-mode :ensure t)

;; =============================================================================
;;                                                                    JavaScript
;; =============================================================================

(defun tape-onlyify ()
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (re-search-backward "^test")
    (forward-sexp)
    (if (looking-at ".only") (progn (zap-to-char 1 (string-to-char "(")) (insert "("))
      (insert ".only"))))

(use-package js2-mode
  :commands (js2-mode)
  :mode "\\.js\\'"
  :bind
  ;; (("C-c b" . web-beautify-js)) TODO: to make this mode specific
  ;; and change binding
  :init
  (progn
    (defvar-setq imalison:identifier-count 0)
    (defun imalison:console-log-unique ()
      (interactive)
      (let* ((identifier-string (int-to-string imalison:identifier-count))
             (uuid (imalison:uuid)))
      (insert (format "console.log('%s//////////%s//////////');" identifier-string uuid))
      (setq imalison:identifier-count (+ imalison:identifier-count 1))))
    (defun imalison:js2-mode-hook ()
      ;; Sensible defaults
      (setq js2-bounce-indent-p nil
            js2-indent-level 4
            js2-basic-offset 4
            js2-highlight-level 3
            js2-include-node-externs t
            js2-mode-show-parse-errors nil
            js2-mode-show-strict-warnings nil
            indent-tabs-mode nil
            js2-indent-switch-body t)
      ;; (edconf-find-file-hook) ;; Make sure that editorconfig takes precedence
      (tern-mode t)
      (when nil (skewer-mode)) ;; TODO: reenable
      (setq imenu-create-index-function
            (lambda ()
              (imalison:flatten-imenu-index
               (js2-mode-create-imenu-index)))))

    (add-hook 'js2-mode-hook 'imalison:js2-mode-hook)
    (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)))

(use-package js2-refactor
  :config
  (progn
    (js2r-add-keybindings-with-prefix "C-c C-m")
    (add-hook 'js2-mode-hook #'js2-refactor-mode)))

(use-package skewer-mode
  :commands skewer-mode
  :config
  (progn
    (add-hook 'css-mode-hook #'skewer-css-mode)
    (add-hook 'html-mode-hook #'skewer-html-mode)))

(use-package tern
  :commands tern-mode
  :config
  (use-package company-tern
    :config (add-to-list 'company-backends 'company-tern)))

(defun delete-tern-process ()
  (interactive)
  (delete-process "tern"))

(use-package json-mode
  :mode "\\.json\\'"
  :init
  (add-hook 'json-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq js-indent-level 4))))

(use-package jsx-mode
  :mode "\\.jsx\\'")

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;; =============================================================================
;;                                                                          Ruby
;; =============================================================================

(use-package robe
  :commands robe-mode
  :init
  (progn (add-hook 'ruby-mode-hook
                   (lambda () (robe-mode)))))

(use-package rinari :ensure t)

;; =============================================================================
;;                                                                         C/C++
;; =============================================================================

;; XXX: None of this looks ruby related. Why is this here?
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)

(use-package helm-gtags
  :disabled t
  :config (custom-set-variables
           '(helm-gtags-path-style 'relative)
           '(helm-gtags-ignore-case t)
           '(helm-gtags-auto-update t))
  :bind
  (("M-t" . helm-gtags-find-tag)
   ("M-r" . helm-gtags-find-rtag)
   ("M-s" . helm-gtags-find-symbol)
   ("C-c <" . helm-gtags-previous-history)
   ("C-c >" . helm-gtags-next-history))
  :init
  (progn
    ;;; Enable helm-gtags-mode
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)))

;; =============================================================================
;;                                                                           TeX
;; =============================================================================

(defun guess-TeX-master (filename)
  "Guess the master file for FILENAME from currently open .tex files."
  (let ((candidate nil)
        (filename (file-name-nondirectory filename)))
    (save-excursion
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (let ((name (buffer-name))
                (file buffer-file-name))
            (if (and file (string-match "\\.tex$" file))
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward
                       (concat "\\\\input{" filename "}") nil t)
                      (setq candidate file))
                  (if (re-search-forward
                       "\\\\include{" (file-name-sans-extension filename) "}"
                       nil t)
                      (setq candidate file))))))))
    (if candidate
        (message "TeX master document: %s" (file-name-nondirectory candidate)))
    candidate))

(defun set-TeX-master ()
    (setq TeX-master (guess-TeX-master (buffer-file-name))))

(use-package tex
  :ensure auctex
  :commands TeX-mode
  :config
  (progn
    (add-hook 'TeX-mode-hook 'set-TeX-master)
    (unbind-key "C-j" TeX-mode-map)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-save-query nil)
    (setq TeX-PDF-mode t)
    (TeX-global-PDF-mode t)
    (setq-default TeX-master nil)))

(use-package latex
  :ensure auctex
  :config
  (progn
    (unbind-key "C-j" LaTeX-mode-map)))

;; =============================================================================
;;                                                                   other modes
;; =============================================================================

(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :preface
  (progn
    (defun go-mode-create-imenu-index ()
      "Create and return an imenu index alist. Unlike the default
alist created by go-mode, this method creates an alist where
items follow a style that is consistent with other prog-modes."
      (let* ((patterns '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)))
             (type-index (imenu--generic-function patterns))
             (func-index))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward go-func-meth-regexp (point-max) t)
            (let* ((var (match-string-no-properties 1))
                   (func (match-string-no-properties 2))
                   (name (if var
                             (concat (substring var 0 -1) "." func)
                           func))
                   (beg (match-beginning 0))
                   (marker (copy-marker beg))
                   (item (cons name marker)))
              (setq func-index (cons item func-index)))))
        (nconc type-index (list (cons "func" func-index))))))
  :config
  (progn
    (use-package company-go
      :config (setq company-go-show-annotation t))
    (use-package go-projectile)
    (use-package go-eldoc)
    (use-package gotest)
    (setq go-test-verbose t)
    (bind-key "M-." 'godef-jump go-mode-map)
    (bind-key "M-," 'pop-tag-mark go-mode-map)
    (imalison:prefix-alternatives
     imalison:gotest go-test-current-test go-test-current-file)
    (defun imalison:go-mode-hook ()
      (go-eldoc-setup)
      (bind-key "C-c t" 'imalison:gotest go-mode-map)
      (setq imenu-create-index-function
            (lambda ()
              (imalison:flatten-imenu-index
               (go-mode-create-imenu-index))))
      (set (make-local-variable 'company-backends) '(company-go)))
    (add-hook 'go-mode-hook 'imalison:go-mode-hook)
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save t)
    (defun go-mode-workspace-path ()
      (file-relative-name (projectile-project-root)
                          (concat (file-name-as-directory
                                   (or (getenv "GOPATH") "~/go")) "src")))
    (defun go-mode-install-current-project ()
      (interactive)
      (start-process "go install" "go install log" "go" "install"
                     (concat (file-name-as-directory (go-mode-workspace-path))
                             "...")))
    (add-hook 'before-save-hook 'go-mode-install-current-project)))

(use-package rust-mode
  :mode (("\\.rs\\'" . rust-mode)))

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(use-package sgml-mode
  :commands sgml-mode
  ;; :bind ("C-c b" . web-beautify-html) TODO: mode specific, change binding
  )

(use-package gitconfig-mode
  :mode "\\.?gitconfig\\'")

(use-package evil :ensure t :commands (evil-mode))

(use-package thrift
  :commands thrift-mode
  :mode (("\\.thrift\\'" . thrift-mode)))

(use-package markdown-mode
  :init
  (progn
    (add-hook 'markdown-mode-hook 'imalison:disable-linum-mode)))

;; =============================================================================
;;                                                           Custom Key Bindings
;; =============================================================================

;; Miscellaneous
(global-unset-key (kbd "C-o")) ;; Avoid collision with tmux binding.
(bind-key "M-q" 'fill-or-unfill-paragraph)
(bind-key "C-c C-s" 'sudo-edit)
(bind-key "C-c SPC"
          (lambda () (interactive)
            (if current-prefix-arg (helm-global-mark-ring) (helm-mark-ring))))
(bind-key "C-c e" 'os-copy)
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)
(bind-key "C-x C-b" 'buffer-menu)
(bind-key "C-x C-c" 'kill-emacs)
(bind-key "C-x C-i" 'imenu)
(bind-key "C-x C-r" (lambda () (interactive) (revert-buffer t t)))
(bind-key "C-x O" (lambda () (interactive) (other-window -1)))
(bind-key "C-x w" 'whitespace-mode)
(bind-key "M-n" 'forward-paragraph)
(bind-key "M-p" 'backward-paragraph)
(bind-key "C-M-<backspace>" 'backward-kill-sexp)
(bind-key "s-<return>" 'toggle-frame-fullscreen)
(bind-key "M-|" 'imalison:shell-command-on-region)
(bind-key "C--" 'undo)
(bind-key "C-c 8" 'imalison:term)
(bind-key "C-c 7" 'imalison:force-new-term)
(bind-key "C-x 9" 'previous-buffer)

(fset 'global-set-key-to-use-package
      (lambda (&optional arg) "Keyboard macro." (interactive "p")
        (kmacro-exec-ring-item
         (quote ([1 67108896 19 100 6 23 40 19 41 return
                    backspace 32 46 6 4] 0 "%d")) arg)))

;; =============================================================================
;;                                                                          toys
;; =============================================================================

(use-package hackernews :ensure t :commands hackernews)

;; =============================================================================
;;                                                                    Appearance
;; =============================================================================

(defvar packages-appearance
  '(monokai-theme solarized-theme zenburn-theme base16-theme molokai-theme
    tango-2-theme gotham-theme sublime-themes ansi-color rainbow-delimiters
    ample-theme material-theme zerodark-theme color-theme-modern))

(ensure-packages-installed packages-appearance)

(setq inhibit-startup-screen t)
(blink-cursor-mode -1)

;; make whitespace-mode use just basic coloring
(setq whitespace-style (quote (spaces tabs newline space-mark
                                      tab-mark newline-mark)))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (tab-mark 9 [9655 9] [92 9])))

(defun colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package window-number)

(use-package spaceline-config
  :ensure spaceline
  :config
  (progn
    (setq spaceline-workspace-numbers-unicode t
          spaceline-window-numbers-unicode t)
    (if (display-graphic-p)
        (setq-default powerline-default-separator 'wave)
      (setq-default powerline-default-separator 'utf-8))
    (setq powerline-height 25)
    (spaceline-spacemacs-theme)))

(use-package telephone-line
  :disabled t
  :config
  (progn
    (setq telephone-line-primary-left-separator #'telephone-line-cubed-left)
    (telephone-line-mode 1)))

;; =============================================================================
;;                                                                        Themes
;; =============================================================================

;; These can be overriden in custom-before.el
(defvar imalison:light-theme 'solarized-light)
(defvar imalison:dark-theme 'material)
(use-package theme-changer
  :config
  (progn
    (destructuring-bind (latitude longitude)
        (imalison:get-lat-long)
      (setq calendar-latitude latitude)
      (setq calendar-longitude longitude))))

(defun random-choice (choices)
  (nth (random (length choices)) choices))

(defun imalison:set-font-height ()
  (interactive)
  (let ((new-height (read-face-attribute 'default :height (selected-frame))))
    (set-face-attribute 'default nil :height new-height)))

(defun imalison:remove-fringe-and-hl-line-mode (&rest stuff)
  (interactive)
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  ;; (set-fringe-mode 0) ;; Lets reenable fringes. They seem useful
  (defvar-setq linum-format 'dynamic)
  (setq left-margin-width 0)
  (defvar-setq hl-line-mode nil))

(defun imalison:after-load-theme (&rest args)
  (when (fboundp 'powerline-reset)
    (powerline-reset))
  (imalison:restore-ansi-term-color-vector))

(when t
  (if (emacs24_4-p)
      (advice-add 'load-theme :after #'imalison:after-load-theme)
    (defadvice load-theme (after name activate)
      (imalison:after-load-theme))))

(when (file-exists-p custom-after-file) (load custom-after-file))

(defvar imalison:ansi-term-color-vector ansi-term-color-vector)

(defun imalison:ansi-term-color-vector-broken? ()
    (--some (or (eq it 'unspecified) (not (symbolp it)))
            (append ansi-term-color-vector nil)))

(defun imalison:restore-ansi-term-color-vector (&optional force)
  (when (or force (imalison:ansi-term-color-vector-broken?))
    (setq ansi-term-color-vector imalison:ansi-term-color-vector)))

(defun imalison:appearance (&optional frame)
  (interactive)
  (message "called set appearance")
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font "Source Code Pro")
        (set-face-attribute 'default nil :weight 'semi-bold)
        (set-face-attribute 'default nil :height 135))
    (progn
      (load-theme 'source-code-pro t)
      (message "not setting font")))
  (load-theme imalison:dark-theme t)
  (imalison:remove-fringe-and-hl-line-mode)
  (message "finished set appearance"))

;; This is needed because you can't set the font or theme at daemon start-up.
;; (when (display-graphic-p) (imalison:appearance))
(add-hook 'after-init-hook 'imalison:appearance)
(add-hook 'after-make-frame-functions 'imalison:appearance)
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
