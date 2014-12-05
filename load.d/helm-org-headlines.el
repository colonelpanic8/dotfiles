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
    :candidates (helm-org-get-candidates filenames)
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
  (helm-org-goto-marker marker)
  (goto-char (marker-position marker))
  (let (destination-level (org-current-level))
    (org-end-of-subtree t t)
    (org-paste-subtree (+ destination-level 1))))

(defun helm-org-get-candidates (filenames)
  (-flatten
   (mapcar (lambda (filename)
             (helm-get-org-candidates-in-file
              filename min-depth max-depth))
           org-agenda-files)))

(defun helm-get-org-candidates-in-file (filename min-depth max-depth)
  (with-current-buffer (find-file-noselect filename)
    (save-excursion
      (beginning-of-buffer)
      (cl-loop while (re-search-forward org-complex-heading-regexp nil t)
               collect `(,(match-string-no-properties 0) . ,(point-marker))))))
