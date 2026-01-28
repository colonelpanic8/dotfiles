;;; custom-config.el --- Container config loader -*- lexical-binding: t; -*-

;; Helper function used by org-config (must be defined before loading preface)
(defun imalison:join-paths (&rest paths)
  "Join PATHS together into a single path."
  (let ((result (car paths)))
    (dolist (p (cdr paths))
      (setq result (expand-file-name p result)))
    result))

;; Load tangled config files in order
(let ((config-dir (file-name-directory load-file-name)))
  ;; Load preface first (defines variables with default values)
  (when (file-exists-p (expand-file-name "org-config-preface.el" config-dir))
    (load (expand-file-name "org-config-preface.el" config-dir)))

  ;; Override paths for container environment AFTER loading preface
  ;; Use setq to ensure we override the defvar values from preface
  (setq imalison:org-dir "/data/org")
  (setq imalison:shared-org-dir "/data/shared")

  ;; Re-derive all path variables using the container org-dir
  (setq imalison:org-gtd-file (imalison:join-paths imalison:org-dir "gtd.org"))
  (setq imalison:org-habits-file (imalison:join-paths imalison:org-dir "habits.org"))
  (setq imalison:org-calendar-file (imalison:join-paths imalison:org-dir "calendar.org"))
  (setq imalison:org-inbox-file (imalison:join-paths imalison:org-dir "inbox.org"))

  ;; Shared paths derived from shared-org-dir
  (setq imalison:shared-org-gtd-file (imalison:join-paths imalison:shared-org-dir "gtd.org"))
  (setq imalison:shared-habits-file (imalison:join-paths imalison:shared-org-dir "habits.org"))
  (setq imalison:shared-calendar-file (imalison:join-paths imalison:shared-org-dir "calendar.org"))
  (setq imalison:shared-shopping-file (imalison:join-paths imalison:shared-org-dir "shopping.org"))
  (setq imalison:shared-repeating-file (imalison:join-paths imalison:shared-org-dir "repeating.org"))
  (setq imalison:orgzly-files (list (imalison:join-paths imalison:org-dir "orgzly.org")))
  (setq imalison:repeating-org-files (list imalison:org-habits-file
                                           imalison:shared-habits-file
                                           imalison:shared-repeating-file))

  ;; org-config-custom.el uses customize format (var value), convert to setq
  (when (file-exists-p (expand-file-name "org-config-custom.el" config-dir))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "org-config-custom.el" config-dir))
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (listp form) (symbolp (car form)))
                (set (car form) (eval (cadr form))))))
        (end-of-file nil))))

  ;; Load main config (sets up org-agenda-files using the variables we just set)
  (when (file-exists-p (expand-file-name "org-config-config.el" config-dir))
    (load (expand-file-name "org-config-config.el" config-dir)))

  ;; Load optional overrides (instance-specific customizations)
  (when (file-exists-p (expand-file-name "overrides.el" config-dir))
    (load (expand-file-name "overrides.el" config-dir))))

;; Define no-op stubs for unavailable packages (overwrite autoloads)
(defun org-bullets-mode (&optional _arg)
  "No-op stub for org-bullets-mode (package not available in container)."
  nil)

;; Override shared-org-file-p to handle nil imalison:shared-org-dir
;; The original calls (file-truename imalison:shared-org-dir) which errors when nil
(defun imalison:shared-org-file-p ()
  "Check if current file is in the shared org directory.
Returns nil if imalison:shared-org-dir is not set."
  (and imalison:shared-org-dir
       (string-prefix-p (file-truename imalison:shared-org-dir)
                        (file-truename default-directory))))

;; Helper functions used by org-agenda-custom-commands
;; These are defined in README.org but needed for custom views
(defun imalison:compare-int-list (a b)
  "Compare two lists of integers lexicographically."
  (when (and a b)
    (cond ((> (car a) (car b)) 1)
          ((< (car a) (car b)) -1)
          (t (imalison:compare-int-list (cdr a) (cdr b))))))

(defun get-date-created-from-agenda-entry (agenda-entry)
  "Get the CREATED property timestamp from an agenda entry."
  (org-time-string-to-time
   (org-entry-get (get-text-property 1 'org-marker agenda-entry) "CREATED")))

;; Auto-convert org-capture-templates to org-agenda-api-capture-templates
(defun imalison:extract-template-string (template-spec)
  "Extract the template string from TEMPLATE-SPEC.
Handles string templates, function templates, and file templates."
  (let ((template-part (nth 4 template-spec)))
    (cond
     ((stringp template-part) template-part)
     ((and (listp template-part)
           (eq (car template-part) 'function))
      ;; Try to evaluate the function to get template string
      (condition-case nil
          (let ((result (funcall (eval (cadr template-part)))))
            (if (stringp result) result ""))
        (error "")))
     ((and (listp template-part)
           (eq (car template-part) 'file))
      ;; File template - read file contents
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents (eval (cadr template-part)))
            (buffer-string))
        (error "")))
     (t ""))))

(defun imalison:infer-prompt-type (prompt-match)
  "Infer the prompt type from PROMPT-MATCH.
PROMPT-MATCH is the full match string like \"%^{Name}t\" or \"%^{Title}\"."
  (let ((suffix (substring prompt-match (1- (length prompt-match)))))
    (cond
     ((string-match-p "[tTuU]$" prompt-match) 'date)
     ((string= "g" suffix) 'tag)
     ((string= "G" suffix) 'tag)
     ((string= "C" suffix) 'string)  ; completion
     (t 'string))))

(defun imalison:extract-prompts-from-template (template-string)
  "Extract prompt definitions from TEMPLATE-STRING.
Returns a list of (NAME :type TYPE :required t) for each %^{...} found."
  (let ((prompts '())
        (seen-names '())
        (pos 0))
    (while (string-match "%\\^{\\([^}|]+\\)\\(?:|[^}]*\\)?}\\([tTuUgGC]\\)?" template-string pos)
      (let* ((name (match-string 1 template-string))
             (full-match (match-string 0 template-string))
             (type (imalison:infer-prompt-type full-match)))
        (unless (member name seen-names)
          (push name seen-names)
          (push (list name :type type :required t) prompts))
        (setq pos (match-end 0))))
    (nreverse prompts)))

(defun imalison:convert-capture-template (template-spec)
  "Convert a single org-capture TEMPLATE-SPEC to API format.
Returns nil for non-entry templates or templates that can't be converted."
  (when (and (listp template-spec)
             (>= (length template-spec) 4))
    (let* ((key (nth 0 template-spec))
           (description (nth 1 template-spec))
           (type (nth 2 template-spec)))
      ;; Only convert entry-type templates, skip menu items (no type)
      (when (and (stringp key)
                 (stringp description)
                 (eq type 'entry))
        (let* ((template-string (imalison:extract-template-string template-spec))
               (prompts (imalison:extract-prompts-from-template template-string))
               ;; Create a unique API key from the hotkey
               (api-key (concat "capture-" key)))
          (list api-key
                :name description
                :template template-spec
                :prompts prompts))))))

(defun imalison:convert-all-capture-templates ()
  "Convert all org-capture-templates to org-agenda-api-capture-templates format."
  (let ((converted '()))
    (dolist (template org-capture-templates)
      (let ((api-template (imalison:convert-capture-template template)))
        (when api-template
          (push api-template converted))))
    (nreverse converted)))

;; Auto-generate API capture templates from org-capture-templates
(setq org-agenda-api-capture-templates (imalison:convert-all-capture-templates))

;;; custom-config.el ends here
