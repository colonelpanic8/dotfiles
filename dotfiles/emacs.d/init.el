;; -*- lexical-binding: t; -*-
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

(defun emacs24_4-p ()
  (or (and (>= emacs-major-version 24)
           (>= emacs-minor-version 4))
      (>= emacs-major-version 25)))

;; =============================================================================
;;                                                                  GUI Disables
;; =============================================================================

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; These silence the byte compiler.
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-context-switch-command nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)

;; =============================================================================
;;                                                       Load Path Configuration
;; =============================================================================

(defvar machine-custom "~/.emacs.d/this-machine.el")
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file) (load custom-file))
(when (file-exists-p machine-custom) (load machine-custom))

;; =============================================================================
;;                                                         ELPA/package.el/MELPA
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
(ensure-packages-installed '(epl use-package))
(require 'use-package)

(use-package benchmark-init
  :ensure t
  ;; Set do-benchmark in custom.el
  :if (and (boundp 'do-benchmark) do-benchmark))

;; =============================================================================
;;                                                          Config Free Packages
;; =============================================================================

(defun use-packages (packages)
  (mapcar
   (lambda (package)
     (use-package package :ensure t)) packages))

(defvar packages-eager
  '(popup auto-complete yasnippet cl-lib paradox slime
    xclip dired+ ctags ctags-update aggressive-indent imenu+ neotree diminish
    gist))

(use-packages packages-eager)

;; =============================================================================
;;                                                                      Disables
;; =============================================================================

(setq visible-bell nil)
(setq sentence-end-double-space nil)

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
(setq flyspell-issue-welcome-flag nil)

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

(defun org-todo-force-notes ()
  (interactive)
  (let ((org-todo-log-states
         (mapcar (lambda (state)
                   (list state 'note 'time))
                 (apply 'append org-todo-sets))))
    (cond ((eq major-mode 'org-mode)  (org-todo))
          ((eq major-mode 'org-agenda-mode) (org-agenda-todo)))))

(defun org-todo-force-notes ()
  (interactive)
  (let ((org-todo-log-states
         (mapcar (lambda (state)
                   (list state 'note 'time))
                 (apply 'append org-todo-sets))))
    (org-todo)
    ))

(defun org-agenda-todo-force-notes ()
  (interactive)
  (let ((org-todo-log-states
         (mapcar (lambda (state)
                   (list state 'note 'time))
                 (apply 'append org-todo-sets))))
    (org-agenda-todo)))

(defun org-todo-no-note ()
  (interactive)
  (org-todo 0))

(defun org-project-heading (heading)
  (interactive
   (list (read-string "Heading: ")))
  (org-insert-or-goto-heading heading)
  (hide-subtree)
  (org-beginning-of-line)
  (org-set-property "CATEGORY" heading))
  
(defun org-insert-or-goto-heading (heading)
  (interactive
   (list (read-string "Heading: ")))
  (goto-char (point-min))
  (unless (derived-mode-p 'org-mode)
    (error
     "Target buffer \"%s\" for file+headline should be in Org mode"
     (current-buffer)))
  (if (re-search-forward
       (format org-complex-heading-regexp-format (regexp-quote heading))
       nil t)
      (goto-char (point-at-bol))
    (goto-char (point-max))
    (or (bolp) (insert "\n"))
    (insert "* " heading)))

(defun org-make-habit ()
  (interactive)
  (org-set-property "STYLE" "habit"))

(defun org-insert-habit ()
  (interactive)
  (org-insert-todo-heading nil)
  (org-make-habit))

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

(defun make-frame-if-none-exists ()
  (let* ((existing-frame (frame-exists)))
    (if existing-frame
        existing-frame
      (make-frame-on-display (getenv "DISPLAY")))))

(defun make-frame-if-none-exists-and-focus ()
  (make-frame-visible (select-frame (make-frame-if-none-exists))))

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

(defvar notify-function 'notification-center)

(defun project-root-of-file (filename)
  "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (file-truename
   (let ((dir (file-truename filename)))
     (or (--reduce-from
          (or acc
              (let* ((cache-key (format "%s-%s" it dir))
                     (cache-value (gethash cache-key projectile-project-root-cache)))
                (if cache-value
                    (if (eq cache-value 'no-project-root)
                        nil
                      cache-value)
                  (let ((value (funcall it dir)))
                    (puthash cache-key (or value 'no-project-root) projectile-project-root-cache)
                    value))))
          nil
          projectile-project-root-files-functions)
         (if projectile-require-project-root
             (error "You're not in a project")
           default-directory)))))

;; =============================================================================
;;                                                         General Emacs Options
;; =============================================================================

;; This makes it so that emacs --daemon puts its files in ~/.emacs.d/server
(setq server-use-tcp t)

;; Display line and column numbers in mode line.
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(setq visible-bell t)
(show-paren-mode 1)

;; Make buffer names unique.
(setq uniquify-buffer-name-style 'forward)

;; We want closures.nil
(setq lexical-binding t)

;; Don't disable downcase and upcase region.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Make forward word understand camel and snake case.
(setq c-subword-mode t)

;; Preserve pastes. Why wouldn't this be enabled by default.
(setq save-interprogram-paste-before-kill t)

(setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows 'bar)

(add-hook 'after-init-hook '(lambda () (setq debug-on-error t)))

;; Make mouse scrolling less jumpy.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(eval-after-load "subword-mode" '(diminish 'subword-mode))

;; Set path from shell.
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package load-dir
  :ensure t
  :config
  (progn
    (add-to-list 'load-dirs "~/.emacs.d/load.d")
    (defvar site-lisp "/usr/share/emacs24/site-lisp/")
    (when (file-exists-p site-lisp) (add-to-list 'load-dirs site-lisp))))

(use-package tramp
  :commands tramp
  :config
  (setq tramp-default-method "ssh"))

;; text mode stuff:
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq sentence-end-double-space nil)

;; y and n instead of yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package discover-my-major :ensure t)

(use-package guide-key
  :ensure t
  :config
  (progn
    (setq guide-key/guide-key-sequence
          '("C-c" "C-c p" "C-x C-k" "C-x r" "C-x c"))
    (guide-key-mode 1)
    (diminish 'guide-key-mode)
    (setq guide-key/idle-delay 0.25)
    (setq guide-key/recursive-key-sequence-flag t)
    (setq guide-key/popup-window-position 'bottom)))

(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :init
  (progn
    (use-package ace-window
      :ensure t
      :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
      :bind ("C-c w" . ace-select-window)))
  :config
  (progn
    (setq ace-jump-mode-scope 'window))
  :bind (("C-j" . ace-jump-mode)))

(use-package ace-isearch
  :ensure t
  :config
  (progn
    (global-ace-isearch-mode +1)
    (setq ace-isearch-use-function-from-isearch nil)
    (setq ace-isearch-input-idle-delay .25)))

(use-package flycheck
  :ensure t
  :commands (flycheck-mode)
  :init (add-hook 'after-init-hook #'flycheck-mode)
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
  (progn
    (diminish 'magit-auto-revert-mode)
    (use-package magit-filenotify
      ;; Seems like OSX does not properly this.
      :disabled t
      :ensure t
      :if (emacs24_4-p)
      :config
      :init (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))))

(use-package auto-complete
  :ensure t
  :commands auto-complete-mode
  :config
  (diminish 'auto-complete-mode)
  :init
  (add-hook 'prog-mode-hook (lambda () (auto-complete-mode t))))

(use-package company
  :ensure t
  :commands company-mode
  :config
  (diminish 'company-mode)
  :init
  (add-hook 'prog-mode-hook (lambda () (company-mode t))))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :config (setq expand-region-contract-fast-key "j")
  :bind (("C-c k" . er/expand-region)))

(use-package multiple-cursors
  :config
  (progn
    (use-package phi-search-mc
      :ensure t
      :config
      (phi-search-mc/setup-keys))
    (use-package mc-extras
      :ensure t
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
  :ensure t
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

(use-package smooth-scrolling
  :ensure t
  :init (require 'smooth-scrolling))

(use-package smooth-scroll
  :ensure t
  :init
  (progn
    (smooth-scroll-mode)
    (setq smooth-scroll/vscroll-step-size 8))
  :config
  (diminish 'smooth-scroll-mode))

(use-package string-inflection
  :ensure t
  :commands string-inflection-toggle
  :bind ("C-c l" . string-inflection-toggle))

;; =============================================================================
;;                                                         Non-Programming Stuff
;; =============================================================================

(use-package helm-spotify
  :ensure t
  :commands helm-spotify)

(use-package edit-server
  :ensure t
  :disabled t
  :commands edit-server-start
  :idle (edit-server-start))

(use-package jabber
  :ensure t
  :commands jabber-connect
  :config
  (progn
    (defun jabber-message-content-message (from buffer text)
      (when (or jabber-message-alert-same-buffer
                (not (memq (selected-window) (get-buffer-window-list buffer))))
        (if (jabber-muc-sender-p from)
            (format "%s: %s" (jabber-jid-resource from) text)
          (format "%s: %s" (jabber-jid-displayname from) text))))
    (setq jabber-alert-message-function 'jabber-message-content-message)))

(use-package org
  :ensure t
  :commands (org-mode org org-mobile-push org-mobile-pull org-agenda)
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c n t" . org-insert-todo-heading)
         ("C-c n s" . org-insert-todo-subheading)
         ("C-c n h" . org-insert-habit)
         ("C-c n m" . org-make-habit)
         ("C-c C-t" . org-todo)
         ("C-c C-S-t" . org-todo-force-notes))
  :config
  (progn
    (setq org-habit-graph-column 50)
    (setq org-habit-show-habits-only-for-today t)
    (unless (boundp 'org-gtd-file)
      (defvar org-gtd-file "~/org/gtd.org"))
    (unless (boundp 'org-habits-file)
      (defvar org-habits-file "~/org/habits.org"))
    (unless (boundp 'org-projects-file)
      (defvar org-projects-file "~/org/projects.org"))
    (unless (boundp 'org-capture-templates)
      (defvar org-capture-templates nil))
    (setq org-completion-use-ido t)
    (setq org-enforce-todo-dependencies t)
    (setq org-agenda-files (list org-gtd-file org-habits-file org-projects-file))
    (add-to-list 'org-capture-templates
                 `("h" "Habit" entry (file+headline ,org-habits-file "Habits")
                   "* TODO 
  SCHEDULED: %t
  :PROPERTIES:
  :STYLE:    habit
  :END:"))

    (add-to-list 'org-capture-templates
                 `("t" "Life Todo" entry (file+headline ,org-gtd-file "Tasks")
                   "* TODO %?\n"))

    (defun org-get-project-name-of-capture-file ()
      (org-project-heading
       (file-name-nondirectory
        (directory-file-name (project-root-of-file
                              (plist-get org-capture-plist :original-file)))))
      (org-end-of-line))

    (add-to-list 'org-capture-templates
                 `("p" "Project Todo" entry
                   (file+function ,org-projects-file org-get-project-name-of-capture-file)
                   "* TODO %?\n"))
    (add-to-list 'org-modules 'org-habit)
    (let ((this-week-high-priority
           '(tags-todo "+PRIORITY=\"A\"+DEADLINE<\"<+1w>\"DEADLINE>\"<+0d>\""
                       ((org-agenda-overriding-header
                         "Upcoming high priority tasks:"))))
          (due-today '(tags-todo
                           "+DEADLINE=<\"<+0d>\""
                           ((org-agenda-overriding-header
                             "Due today:"))))
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
               (,due-today
                ,this-week-high-priority
                (agenda ""
                        ((org-agenda-overriding-header "Agenda:")
                         (org-agenda-ndays 5)
                         (org-deadline-warning-days 0)))
                ,missing-deadline
                ,missing-priority)
               nil nil)
              ,(cons "A" (cons "High priority upcoming" this-week-high-priority))
              ,(cons "od" (cons "Overdue tasks and due today" due-today)))))
    ;; Record changes to todo states
    (setq org-log-into-drawer t)
    (setq org-todo-keywords
          '((sequence "TODO(t!)" "STARTED(s!)" "WAIT(w!)" "|"
                      "DONE(d!)" "CANCELED(c!)")))
    ;; Stop starting agenda from deleting frame setup!
    (setq org-agenda-window-setup 'other-window)
    (define-key mode-specific-map [?a] 'org-agenda)
    (unbind-key "C-j" org-mode-map))
  :init
  (progn
    ;; Automatically sync with mobile
    (defvar my-org-mobile-sync-timer nil)
    (defvar my-org-mobile-sync-secs 120)
    (defun my-org-mobile-sync-pull-and-push ()
      (org-mobile-pull)
      (org-mobile-push)
      (when (fboundp 'sauron-add-event)
        (sauron-add-event 'me 1 "Called org-mobile-pull and org-mobile-push")))
    (defun my-org-mobile-sync-start ()
      "Start automated `org-mobile-push'"
      (interactive)
      (setq my-org-mobile-sync-timer
            (run-with-idle-timer my-org-mobile-sync-secs t
                                 'my-org-mobile-sync-pull-and-push)))

    (defun my-org-mobile-sync-stop ()
      "Stop automated `org-mobile-push'"
      (interactive)
      (cancel-timer my-org-mobile-sync-timer))
    (if (and (boundp 'file-notify--library) file-notify--library)
        (use-package org-mobile-sync :ensure t :config (org-mobile-sync-mode 1))
      (my-org-mobile-sync-start))
    (setq org-directory "~/Dropbox/org")
    (setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
    (defun guide-key/my-hook-function-for-org-mode ()
      (guide-key/add-local-guide-key-sequence "C-c")
      (guide-key/add-local-guide-key-sequence "C-c C-x")
      (guide-key/add-local-highlight-command-regexp "org-"))
    (add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)
    (defun disable-linum-mode () (linum-mode 0))
    (add-hook 'org-mode-hook 'disable-linum-mode)
    (add-hook 'org-agenda-mode-hook 'disable-linum-mode)))

(use-package epg
  :ensure t
  :config
  (epa-file-enable))

(use-package twittering-mode
  :ensure t
  :commands twittering-mode)

(use-package erc
  :ensure t
  :commands erc
  :config
  (progn
    (use-package erc-colorize :ensure t) (erc-colorize-mode 1)))

(use-package sauron
  :ensure t
  :commands (sauron-start sauron-start-hidden)
  :config
  (progn
    (setq sauron-min-priority 3)
    (setq sauron-separate-frame nil)
    (setq sauron-nick-insensitivity 1)
    (defun sauron:jabber-notify (origin priority message &optional properties)
      (funcall notify-function "gtalk" message))
    (defun sauron:erc-notify (origin priority message &optional properties)
      (let ((event (plist-get properties :event)))
            (funcall notify-function "IRC" message)))
    (defun sauron:mu4e-notify (origin priority message &optional properties)
      nil)
    (defun sauron:dispatch-notify (origin priority message &optional properties)
      (let ((handler (cond ((string= origin "erc") 'sauron:erc-notify)
                            ((string= origin "jabber") 'sauron:jabber-notify)
                            ((string= origin "mu4e") 'sauron:mu4e-notify)
                            (t (lambda (&rest r) nil)))))
        (funcall handler origin priority message properties)))
    (add-hook 'sauron-event-added-functions 'sauron:dispatch-notify))
  :idle (sauron-start-hidden)
  :idle-priority 3)

(use-package flyspell
  :ensure t
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
(add-hook 'prog-mode-hook (lambda () (subword-mode t)))
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; (add-hook 'prog-mode-hook (lambda () (highlight-lines-matching-regexp
;;                                  ".\\{81\\}" 'hi-blue)))

;; =============================================================================
;;                                          File Navigation: helm/projectile/ido
;; =============================================================================

(use-package helm-config
  :ensure helm
  :bind (("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x C-i" . helm-imenu)
         ("C-h a" . helm-apropos)
         ("C-c C-h" . helm-org-agenda-files-headings))
  :init
  (progn
    (require 'helm)
    (helm-mode 1)
    (use-package helm-ag :ensure t :defer t))
  :config
  (progn
    (helm-mode 1)
    (diminish 'helm-mode)))

(use-package helm-swoop :ensure t :commands helm-swoop)

(use-package projectile
  :ensure t
  :commands projectile-global-mode
  :idle (projectile-global-mode)
  :idle-priority 1
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (diminish 'projectile-mode))
  :bind (("C-x f" . projectile-find-file-in-known-projects))
  :init
  (progn
    (use-package persp-projectile
      :ensure t
      :commands persp-projectile)
    (use-package helm-projectile
      :ensure t
      :commands (helm-projectile-on)
      :defer t)))

(use-package smex
  :ensure t
  ;; Using helm-M-x instead
  :disabled t
  :commands smex
  ;; This is here because smex feels like part of ido
  :bind ("M-x" . smex))

(use-package ido
  :ensure t
  :commands ido-mode
  :config
  (progn
    (ido-everywhere 1)
    (setq ido-enable-flex-matching t)
    (use-package flx :ensure t)
    (use-package flx-ido
      :commands flx-ido-mode
      :ensure t
      :init (flx-ido-mode 1)
      :config
      (progn
	;; disable ido faces to see flx highlights.
	;; This makes flx-ido much faster.
	(setq gc-cons-threshold 20000000)
        (setq ido-use-faces nil)))
    (use-package ido-ubiquitous
      :ensure t
      :commands (ido-ubiquitous-mode))
    (use-package ido-vertical-mode
      :ensure t
      :config (ido-vertical-mode 1))
    (use-package flx-ido :ensure t)
    (ido-ubiquitous-mode 1)))

(if (and (boundp 'use-ido) use-ido) (ido-mode))

;; =============================================================================
;;                                                                         elisp
;; =============================================================================

(setq edebug-trace t)

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
(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))
(bind-key "C-c C-f" 'find-function)
(bind-key "C-c C-v" 'find-variable)
(define-key lisp-mode-shared-map (kbd "C-c C-c") 'eval-defun)
(define-key lisp-mode-shared-map (kbd "C-c C-r") 'eval-and-replace)
(define-key lisp-mode-shared-map (kbd "C-c o s") 'up-list-region)
(define-key lisp-mode-shared-map (kbd "C-c o o") 'up-list-back)

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
  (if (projectile-project-p)
      (condition-case ex
          (let ((project-root (projectile-project-root)))
            (cl-remove-if-not 'file-exists-p
                              (mapcar (lambda (env-suffix)
                                        (concat project-root env-suffix))
                                      '(".tox/py27/" "env" ".tox/venv/"))))
        ('error
         (message (format "Caught exception: [%s]" ex))
         (setq retval (cons 'exception (list ex))))
        nil)))

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
      :config
      (progn
        (setq jedi:complete-on-dot t)
        (setq jedi:install-imenu t)
        (setq jedi:imenu-create-index-function 'jedi:create-flat-imenu-index))
      :ensure t
      :bind ("M-." . jedi:goto-definition))
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
      :ensure t
      :config
      (progn
        (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
        (defun guide-key/scala-mode-hook ()
          (guide-key/add-local-guide-key-sequence "C-c C-v"))
        (add-hook 'scala-mode-hook 'guide-key/scala-mode-hook)))
    (setq scala-indent:align-parameters t))
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sc\\'" . scala-mode))
  :ensure t)

;; =============================================================================
;;                                                                    JavaScript
;; =============================================================================

(use-package js2-mode
  :ensure t
  :commands (js-mode)
  :bind
  (("C-c b" . web-beautify-js))
  :init
  (progn
    (use-package skewer-mode
      :ensure t
      :commands skewer-mode)
    (add-hook 'js-mode-hook 'js2-minor-mode)
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
    (add-hook 'js2-mode-hook 'skewer-mode)
    (add-hook 'js2-mode-hook (lambda () (setq js-indent-level 1)))
    (use-package tern
      :commands tern-mode
      :ensure t
      :config
      (progn (tern-ac-setup))
      :init
      (progn
	(use-package tern-auto-complete :ensure t
	  :commands tern-ac-setup)))))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :init
  (add-hook 'json-mode-hook
            (lambda ()
            (setq js-indent-level 2))))

(add-hook 'css-mode-hook #'skewer-css-mode)
(add-hook 'html-mode-hook #'skewer-html-mode)

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;; =============================================================================
;;                                                                          Ruby
;; =============================================================================

(use-package robe
  :ensure t
  :commands robe-mode
  :init
  (progn (add-hook 'ruby-mode-hook 'robe-mode)
         (add-hook 'robe-mode-hook 'ac-robe-setup)
         (add-hook 'ruby-mode-hook 'auto-complete-mode)))

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
  :commands TeX-mode
  :config
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-save-query nil)
    (setq-default TeX-master nil)))

;; =============================================================================
;;                                                                   other modes
;; =============================================================================

(use-package rust-mode :ensure t
  :mode (("\\.rs\\'" . rust-mode)))

(use-package yaml-mode :ensure t
  :mode (("\\.yaml\\'" . yaml-mode)
	 ("\\.yml\\'" . yaml-mode)))

(use-package sgml-mode
  :ensure t
  :commands sgml-mode
  :bind ("C-c b" . web-beautify-html))

(use-package gitconfig-mode
  :ensure t
  :mode "\\.?gitconfig\\'")

(use-package evil :ensure t :commands (evil-mode))

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
(bind-key "M-g" 'goto-line)
(bind-key "M-n" 'forward-paragraph)
(bind-key "M-p" 'backward-paragraph)
(bind-key "M-z" 'zap-to-char)

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
    ample-theme))

(ensure-packages-installed packages-appearance)

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/theme 'respectful)
    (sml/setup)))

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

;; =============================================================================
;;                                                                        Themes
;; =============================================================================

(unless (boundp 'dark-themes)
  (defvar dark-themes '(solarized-dark)))
(unless (boundp 'light-themes)
  (defvar light-themes '(solarized-light)))
(unless (boundp 'terminal-themes)
  (defvar terminal-themes '(solarized-light monokai)))
(unless (boundp 'fonts)
  (defvar fonts '(monaco-9)))

(defun random-choice (choices)
  (nth (random (length choices)) choices))

(defun get-appropriate-theme ()
  (if t ;; (display-graphic-p) why doesn't this work at frame startup?
      (let ((hour
             (string-to-number (format-time-string "%H"))))
        (if (or (< hour 8) (> hour 16))
            (random-choice dark-themes) (random-choice light-themes)))
    (random-choice terminal-themes)))


(setq current-theme nil)

(defun set-theme ()
  (interactive)
  (let ((appropriate-theme (get-appropriate-theme)))
        (if (eq appropriate-theme current-theme)
            nil
          (progn
            (disable-and-load-theme appropriate-theme t)
            (setq current-theme appropriate-theme)))))

(defun disable-all-themes ()
  (interactive)
(mapcar
 (lambda (theme) (unless (s-contains? "smart-mode" (symbol-name theme))
                   (disable-theme theme))) custom-enabled-themes))

(defun disable-and-load-theme (theme &optional no-confirm no-enable)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
			     (mapcar 'symbol-name
				     (custom-available-themes))))
    nil nil))
  (disable-all-themes)
  (load-theme theme no-confirm no-enable))

(defun set-my-font-for-frame (frame)
  (condition-case exp
      (set-frame-font (random-choice fonts) nil t)
    ('error (package-refresh-contents)
	    (set-frame-font "Monaco for Powerline-11" nil t) nil)))

(defun remove-fringe-and-hl-line-mode (&rest stuff)
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (set-fringe-mode 0)
  (setq linum-format 'dynamic)
  (setq left-margin-width 0)
  (set-my-font-for-frame nil)
  (setq hl-line-mode nil))

(if (emacs24_4-p)
    (advice-add 'load-theme :after #'remove-fringe-and-hl-line-mode)
  (defadvice load-theme (after name activate)
    (remove-fringe-and-hl-line-mode)))


;; enable to set theme based on time of day.
(run-at-time "00:00" 3600 'set-theme)

;; This is needed because you can't set the font at daemon start-up.
(add-hook 'after-make-frame-functions 'set-my-font-for-frame)
(add-hook 'after-make-frame-functions (lambda (frame) (set-theme)))
