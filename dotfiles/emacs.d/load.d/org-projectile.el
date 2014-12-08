(defvar org-projectile:projects-file "~/org/projects.org")

(defun org-projectile:project-todo-entry (&optional todo-format)
  (unless todo-format (setq todo-format "* TODO %?\n"))
  `("p" "Project Todo" entry
    (file+function ,org-projectile:projects-file
                   (lambda () (let ((heading (org-projectile:insert-heading-for-filename
                                             (org-capture-get :original-file))))
                                (org-projectile:insert-or-goto-heading heading)
                                (org-end-of-line)
                                heading)))
    ,todo-format))

(defun org-projectile:project-heading-from-file (filename)
  (file-name-nondirectory
   (directory-file-name (project-root-of-file filename))))

(defun org-projectile:insert-heading-for-filename (filename)
  (let ((project-heading
         (org-projectile:project-heading-from-file
          filename)))
    (with-current-buffer (find-file-noselect org-projectile:projects-file)
      (org-projectile:project-heading project-heading))
    project-heading))

(defun org-projectile:known-projects ()
  (delete-dups `(,@(mapcar #'org-projectile:project-heading-from-file (projectile-relevant-known-projects))
                 ,@(org-map-entries (lambda () (nth 4 (org-heading-components))) nil (list org-projectile:projects-file)
                                    (lambda () (when (< 1 (nth 1 (org-heading-components))) (point)))))))


(defun org-projectile:project-todo-completing-read ()
  (interactive)
  (org-projectile:capture-for-project
   (projectile-completing-read "Record TODO for project:"
                               (org-projectile:known-projects))))

(defun org-projectile:capture-for-project (heading)
  (org-capture-set-plist (org-projectile:project-todo-entry))
  (with-current-buffer (find-file-noselect org-projectile:projects-file)
    (org-projectile:project-heading heading))  
  (org-capture-set-target-location `(file+headline ,org-projectile:projects-file ,heading))
  (org-capture-place-template))

(defun org-projectile:insert-or-goto-heading (heading)
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

(defun org-projectile:project-heading (heading)
  (interactive
   (list (read-string "Heading: ")))
  (org-projectile:insert-or-goto-heading heading)
  (hide-subtree)
  (org-beginning-of-line)
  (org-set-property "CATEGORY" heading))

(require 'org-capture)
(require 'projectile)
