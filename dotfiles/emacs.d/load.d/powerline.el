(eval-when-compile (require 'use-package)) ;; use-package is only needed at compile time.
(defmacro spacemacs|custom-flycheck-lighter (error)
        "Return a formatted string for the given ERROR (error, warning, info)."
        `(let* ((error-counts (flycheck-count-errors
                               flycheck-current-errors))
                (errorp (flycheck-has-current-errors-p ',error))
                (err (or (cdr (assq ',error error-counts)) "?"))
                (running (eq 'running flycheck-last-status-change)))
           (if (or errorp running) (format "•%s " err))))

(defvar dotspacemacs-mode-line-unicode-symbols nil)
(use-package powerline
  :ensure t
  :init
  (progn
    ;; Custom format of minor mode lighters, they are separated by a pipe.
    (defpowerline spacemacs-powerline-minor-modes
      (mapconcat (lambda (mm)
                   (propertize
                    mm
                    'mouse-face 'mode-line-highlight
                    'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                    'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map
                                   [mode-line down-mouse-1]
                                   (powerline-mouse 'minor 'menu mm))
                                 (define-key map
                                   [mode-line mouse-2]
                                   (powerline-mouse 'minor 'help mm))
                                 (define-key map
                                   [mode-line down-mouse-3]
                                   (powerline-mouse 'minor 'menu mm))
                                 (define-key map
                                   [header-line down-mouse-3]
                                   (powerline-mouse 'minor 'menu mm))
                                 map)))
                 (split-string (format-mode-line minor-mode-alist))
                 (concat (propertize
                          (if dotspacemacs-mode-line-unicode-symbols " " "") 'face face)
                         (unless dotspacemacs-mode-line-unicode-symbols "|"))))

    (defpowerline spacemacs-powerline-new-version
      (propertize
       spacemacs-version-check-lighter
       'mouse-face 'mode-line-highlight
       'help-echo (format "New version %s | Click with mouse-1 to update (Not Yet Implemented)"
                          spacemacs-new-version)
       'local-map (let ((map (make-sparse-keymap)))
                    (define-key map
                      [mode-line down-mouse-1]
                      (lambda (event) (interactive "@e") (message "TODO: update"))
                      )
                    map)))

    (defvar spacemacs-mode-line-minor-modesp t
      "If not nil, minor modes lighter are displayed in the mode-line.")
    (defun spacemacs/mode-line-minor-modes-toggle ()
      "Toggle display of minor modes."
      (interactive)
      (if spacemacs-mode-line-minor-modesp
          (setq spacemacs-mode-line-minor-modesp nil)
        (setq spacemacs-mode-line-minor-modesp t)))

    (defvar spacemacs-mode-line-new-version-lighterp t
      "If not nil, new version lighter is displayed in the mode-line.")
    (defun spacemacs/mode-line-new-version-lighter-toggle ()
      "Toggle display of new version lighter."
      (interactive)
      (if spacemacs-mode-line-new-version-lighterp
          (setq spacemacs-mode-line-new-version-lighterp nil)
        (setq spacemacs-mode-line-new-version-lighterp t)))

    (defvar spacemacs-mode-line-display-point-p nil
      "If not nil, display point alongside row/column in the mode-line.")
    (defun spacemacs/mode-line-display-point-toggle ()
      (interactive)
      (if spacemacs-mode-line-display-point-p
          (setq spacemacs-mode-line-display-point-p nil)
        (setq spacemacs-mode-line-display-point-p t)))

    (defvar spacemacs-mode-line-org-clock-current-taskp nil
      "If not nil, the currently clocked org-mode task will be
displayed in the mode-line.")
    (defvar spacemacs-mode-line-org-clock-format-function
      'org-clock-get-clock-string
      "Function used to render the currently clocked org-mode task.")
    (defun spacemacs/mode-line-org-clock-current-task-toggle ()
      (interactive)
      (if spacemacs-mode-line-org-clock-current-taskp
          (setq spacemacs-mode-line-org-clock-current-taskp nil)
        (setq spacemacs-mode-line-org-clock-current-taskp t)))

    (if (display-graphic-p)
        (setq-default powerline-default-separator 'wave)
      (setq-default powerline-default-separator 'utf-8))
    (setq powerline-default-separator 'wave)

    (defun spacemacs/customize-powerline-faces ()
      "Alter powerline face to make them work with more themes."
      (set-face-attribute 'powerline-inactive2 nil
                          :inherit 'font-lock-comment-face))
    (spacemacs/customize-powerline-faces)


    (defun spacemacs/mode-line-prepare-left ()
      (let* ((active (powerline-selected-window-active))
             (line-face (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (state-face face2)
             (eyebrowsep (bound-and-true-p eyebrowse-mode))
             (window-numberingp (bound-and-true-p window-numbering-mode))
             (anzup (and (boundp 'anzu--state) anzu--state))
             (flycheckp (and (bound-and-true-p flycheck-mode)
                             (or flycheck-current-errors
                                 (eq 'running flycheck-last-status-change))))
             (vc-face (if (or flycheckp spacemacs-mode-line-minor-modesp)
                          face1 line-face))
             (separator-left (intern (format "powerline-%s-%s"
                                             powerline-default-separator
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              powerline-default-separator
                                              (cdr powerline-default-separator-dir)))))

        (append
         ;; workspace number
         (when (and eyebrowsep (spacemacs/workspace-number))
           (list (powerline-raw " " state-face)
                 (powerline-raw (spacemacs/workspace-number) state-face)))
         ;; window number
         (if (and window-numberingp (spacemacs/window-number))
             (list (if eyebrowsep
                       (powerline-raw "|" state-face)
                     (powerline-raw " " state-face))
                   (powerline-raw (spacemacs/window-number) state-face)
                   (powerline-raw " " state-face))
           (list (powerline-raw "" state-face)))
         (if (and active anzup)
             (list (funcall separator-right state-face face1)
                   (powerline-raw (anzu--update-mode-line) face1)
                   (funcall separator-right face1 line-face))
           (list (funcall separator-right state-face line-face)))
         ;; evil state
         ;; (powerline-raw evil-mode-line-tag state-face)
         ;; (funcall separator-right state-face line-face)
         ;; buffer name
         (list
          (powerline-raw "%*" line-face 'l)
          (powerline-buffer-size line-face 'l)
          (powerline-buffer-id line-face 'l)
          (powerline-raw " " line-face)
          ;; major mode
          (funcall separator-left line-face face1)
          (powerline-major-mode face1 'l)
          (powerline-raw " " face1)
          (when active
            (funcall separator-right face1 line-face)))
         ;; flycheck
         (when (and active flycheckp)
           (list (powerline-raw " " line-face)
                 (powerline-raw (spacemacs|custom-flycheck-lighter error))
                 (powerline-raw (spacemacs|custom-flycheck-lighter warning))
                 (powerline-raw (spacemacs|custom-flycheck-lighter info))))
         ;; separator between flycheck and minor modes
         (when (and active flycheckp spacemacs-mode-line-minor-modesp)
           (list (funcall separator-left line-face face1)
                 (powerline-raw "  " face1)
                 (funcall separator-right face1 line-face)))
         ;; minor modes
         (when (and active spacemacs-mode-line-minor-modesp)
           (list (spacemacs-powerline-minor-modes line-face 'l)
                 (powerline-raw mode-line-process line-face 'l)
                 (powerline-raw " " line-face)))
         ;; erc
         (when (and active
                    (boundp 'erc-track-mode))
           ;; Copied from erc-track.el -> erc-modified-channels-display
           (let* ((buffers (mapcar 'car erc-modified-channels-alist))
                  (long-names (mapcar #'(lambda (buf) (or (buffer-name buf) "")) buffers)))
             long-names))
         ;; version control
         (when (and active (or flycheckp spacemacs-mode-line-minor-modesp))
           (list (funcall separator-left (if vc-face line-face face1) vc-face)))
         (if active
             (list (powerline-vc vc-face)
                   (powerline-raw " " vc-face)
                   (funcall separator-right vc-face face2))
           (list (funcall separator-right face1 face2)))
         ;; org-pomodoro current pomodoro
         (when (and active
                    (fboundp 'org-pomodoro-active-p)
                    (org-pomodoro-active-p))
           org-pomodoro-mode-line)
         ;; org clocked task
         (when (and active
                    spacemacs-mode-line-org-clock-current-taskp
                    (fboundp 'org-clocking-p)
                    (org-clocking-p))
           (list (powerline-raw " " face2)
                 (funcall spacemacs-mode-line-org-clock-format-function)
                 (powerline-raw " " face2))))))

    (defun column-number-at-pos (pos)
      "Analog to line-number-at-pos."
      (save-excursion (goto-char pos) (current-column)))

    (defun selection-info ()
      "Info on the current selection for the mode-line.

It is a string holding:
- the number of columns in the selection if it covers only one line,
- the number of lines in the selection if if covers several full lines
- or rowsxcols if it's a block selection."
      (let* ((lines (count-lines (region-beginning) (1+ (region-end))))
             (chars (- (1+ (region-end)) (region-beginning)))
             (cols (1+ (abs (- (column-number-at-pos (region-end))
                               (column-number-at-pos (region-beginning)))))))
        (if (eq evil-visual-selection 'block)
            (format "%dx%d block" lines cols)
          (if (> lines 1) (format "%d lines" lines)
            (format "%d chars" chars)))))

    (defun spacemacs/mode-line-prepare-right ()
      (let* ((active (powerline-selected-window-active))
             (line-face (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (state-face face2)
             (nyancatp (and (boundp 'nyan-mode) nyan-mode))
             (batteryp (and (boundp 'fancy-battery-mode)
                            (symbol-value fancy-battery-mode)))
             (battery-face (if batteryp (fancy-battery-powerline-face)))
             (separator-left (intern (format "powerline-%s-%s"
                                             powerline-default-separator
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              powerline-default-separator
                                              (cdr powerline-default-separator-dir)))))
        (append
         ;; battery
         (if (and active batteryp)
             (list (funcall separator-left face2 battery-face)
                   (powerline-raw (fancy-battery-default-mode-line)
                                  battery-face 'r)
                   (funcall separator-right battery-face face1))
           (list (funcall separator-right face2 face1)))
         (list
          ;; row:column
          (powerline-raw " " face1)
          (powerline-raw (if spacemacs-mode-line-display-point-p
                             (concat (format "%d | " (point)) "%l:%2c" )
                           "%l:%2c")
                         face1 'r)
          (funcall separator-left face1 line-face)
          (powerline-raw " " line-face))
         (list
          ;; global-mode
          (when active
            (powerline-raw global-mode-string)
            (powerline-raw " " line-face)))
         (when (and active (not nyancatp))
           (let ((progress (format-mode-line "%p")))
             (list
              ;; percentage in the file
              (powerline-raw "%p" line-face 'r)
              ;; display hud
              (powerline-chamfer-left line-face face1)
              (if (string-match "\%" progress)
                  (powerline-hud state-face face1)))))
         )))

    (defun spacemacs/mode-line-prepare ()
      (let* ((active (powerline-selected-window-active))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (lhs (spacemacs/mode-line-prepare-left))
             (rhs (spacemacs/mode-line-prepare-right))
             (nyancatp (and (boundp 'nyan-mode) nyan-mode)))
        (concat (powerline-render lhs)
                (when (and active nyancatp)
                  (powerline-render (spacemacs/powerline-nyan-cat)))
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs))))

    (setq-default mode-line-format
                  '("%e" (:eval (spacemacs/mode-line-prepare))))

    (defun spacemacs//restore-powerline (buffer)
      "Restore the powerline in buffer"
      (with-current-buffer buffer
        (setq-local mode-line-format
                    '("%e" (:eval (spacemacs/mode-line-prepare))))
        (powerline-set-selected-window)
        (powerline-reset)))

    (defun spacemacs//set-powerline-for-startup-buffers ()
      "Set the powerline for buffers created when Emacs starts."
      (unless configuration-layer-error-count
        (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
          (when (get-buffer buffer)
            (spacemacs//restore-powerline buffer)))))
    (add-hook 'after-init-hook
              'spacemacs//set-powerline-for-startup-buffers)))
