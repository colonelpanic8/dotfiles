(defun imalison:copyq-get (i)
  (imalison:shell-command-to-string (format "copyq eval read(%s)" i)))

(defun imalison:copyq-sync ()
  (interactive)
  (let ((missing-items (cl-loop for i from 0 to (string-to-int
                         (imalison:shell-command-to-string "copyq eval size()"))
         for item = (imalison:copyq-get i)
         when (not (member item kill-ring))
         collect item)))
    (setq kill-ring (nconc kill-ring missing-items))))
