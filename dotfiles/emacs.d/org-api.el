(let ((current-dir (if load-file-name (file-name-directory load-file-name) "~/.emacs.d/")))
  (load (expand-file-name "init.el" current-dir)))

(use-package simple-httpd
  :demand t)

(require 'simple-httpd)

(defun org-api-get-todo-elements-from-filepath (filepath)
  (let ((todo-elements nil))
    (with-current-buffer (find-file-noselect filepath)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-todo-regexp nil t)
          (let* ((element (org-element-at-point))
                 (type (org-element-type element)))
            (when (eq type 'headline)
              (let ((todo (org-element-property :todo-keyword element)))
                (when todo
                  (push element todo-elements))))))))
    todo-elements))

(defun org-api-get-agenda-todos ()
  (let* ((today (calendar-gregorian-from-absolute (org-today))))
    (mapcan 'org-api-get-todo-elements-from-filepath org-agenda-files)))

(defun org-api-get-element-json (element)
  (let ((todo (org-element-property :todo-keyword element))
        (title (org-element-property :raw-value element))
        (tags (org-element-property :tags element))
        (level (org-element-property :level element))
        (scheduled (org-element-property :scheduled element))
        (deadline (org-element-property :deadline element)))
    `(("todo" . ,todo)
      ("title" . ,title)
      ("tags" . ,tags)
      ("level" . ,level)
      ("scheduled" . ,(when scheduled (org-format-timestamp scheduled "%Y-%m-%dT%H:%M:%SZ")))
      ("deadline" . ,(when deadline (org-format-timestamp deadline "%Y-%m-%dT%H:%M:%SZ"))))))

(defun org-api-item-json (item)
  (let* ((todo (get-text-property 0 'todo-state item))
        (title (substring-no-properties item))
        (tags (get-text-property 0 'tags item))
        (s (get-text-property 0 'ts-date item))
        (scheduled (when s
                     (org-format-timestamp (org-time-from-absolute s) "%Y-%m-%dT%H:%M:%SZ") )))
    `(("todo" . ,todo)
      ("title" . ,title)
      ("tags" . ,tags))))


(defun org-api-get-scheduled-or-deadlined (day filepath)
  (with-current-buffer (find-file-noselect filepath)
    (org-dlet ((date day))
      (setf org-agenda-current-date date)
      (nconc (org-agenda-get-deadlines) (org-agenda-get-scheduled)))))

(defun org-api-get-today-agenda ()
  (let ((day (calendar-current-date)))
    (mapcan (lambda (filepath) (org-api-get-scheduled-or-deadlined day filepath))
            org-agenda-files)))

(defservlet get-all-todos application/json ()
  (insert (json-encode (mapcar 'org-api-get-element-json (org-api-get-agenda-todos)))))

(defservlet get-todays-agenda application/json ()
  (insert
   (json-encode
    (mapcar 'org-api-item-json
            (org-api-get-today-agenda)))))

(defservlet create-todo application/json (a b c)
  (org-api-capture (gethash "title" (json-parse-string (cadr (assoc "Content" c))))))

(cl-defun org-api-build-capture-template
  (content &key (character "d") (heading "Dynamic"))
    `(,character ,heading entry (file "~/org/inbox.org")
                 (function (lambda ()
                             (imalison:make-org-todo-template :content content)))
                 :immediate-finish t))

(defun org-api-capture (content)
  (let* ((org-capture-templates (list (org-api-build-capture-template content))))
    (org-capture nil "d")))


(message "Starting http server")
(setq httpd-port 2025)
(httpd-start)
(message "Finished starting http server")
