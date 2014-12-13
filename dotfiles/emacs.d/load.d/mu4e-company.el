(defun mu4e-company-contacts (command &optional input &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'mu4e-contacts-company))
    (prefix
     (when (eq major-mode 'mu4e-compose-mode)
       (save-excursion
         (beginning-of-line)
         (if (and (eq (line-number-at-pos) 2)
                  (looking-at "^To: *\\(.*?\\)$"))
             (match-string-no-properties 1)
           'stop))))
    (candidates
     (--filter (string-match-p (downcase input) (downcase it))
               mu4e~contacts-for-completion))))
