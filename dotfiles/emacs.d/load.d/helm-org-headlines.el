(defun helm-org-agenda-files-headlines (&optional min-depth max-depth)
  (interactive)
  (helm :sources (helm-source-org-headlines-for-files org-agenda-files)))

(defun helm-org-goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
  (org-show-entry))


(defun helm-source-org-headlines-for-files (filenames &optional min-depth max-depth)
  (unless min-depth (setq min-depth 1))
  (unless max-depth (setq max-depth 8))
  (helm-build-sync-source "Org Headlines"
    :candidates (helm-org-get-candidates filenames min-depth max-depth)
    :action 'helm-org-goto-marker
    :action-transformer
    (lambda (actions candidate)
      '(("Go to line" . helm-org-goto-marker)
        ("Refile to this headline" . helm-org-headline-refile)
        ("Insert link to this headline"
         . helm-org-headline-insert-link-to-headline)))))

(defun helm-org-headline-refile (marker)
  (with-helm-current-buffer
    (org-cut-subtree))
  (let ((target-level (with-current-buffer (marker-buffer marker)
                       (goto-char (marker-position marker))
                       (org-current-level))))
    (helm-org-goto-marker marker)
    (org-end-of-subtree t t)
    (org-paste-subtree (+ target-level 1))))

(defun helm-org-get-candidates (filenames min-depth max-depth)
  (-flatten
   (mapcar (lambda (filename)
             (helm-get-org-candidates-in-file
              filename min-depth max-depth))
           filenames)))

(defun helm-get-org-candidates-in-file (filename min-depth max-depth)
  (with-current-buffer (find-file-noselect filename)
    (save-excursion
      (beginning-of-buffer)
      (cl-loop while (re-search-forward org-complex-heading-regexp nil t)
               if (let ((num-stars (length (match-string-no-properties 1))))
                    (and (>= num-stars min-depth) (<= num-stars max-depth)))
               collect `(,(match-string-no-properties 0) . ,(point-marker))))))
