(defun imalison:get-args-location-or-eol ()
  (save-excursion
    (end-of-line)
    (let ((bound (point)))
      (beginning-of-line)
      (or (let ((value (re-search-forward "\(" bound t)))
            (when value (- value 1))) (progn (end-of-line) (point))))))

(defun imalison:js-get-params-string ()
  (save-excursion
    (end-of-line)
    (let ((bound (point)))
      (beginning-of-line)
      (if (re-search-forward "\(" bound t)
          (let ((start (progn (backward-char) (point)))
                (end (progn (forward-sexp) (point))))
            (buffer-substring (+ start 1) (- end 1)))
        ""))))

(defun imalison:js-get-left-name ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\.?\\(\[^ .:\]*\\)\[ =:\n\]")
    (match-string-no-properties 1)))

(defun imalison:js-sync-function-names ()
  (interactive)
  (save-excursion
    (let ((params-start (imalison:get-args-location-or-eol))
          (end (progn (end-of-line) (point))))
      (beginning-of-line)
      (or (re-search-forward "\[ =:\]+" end t)
          (end-of-line))
      (delete-region (point) params-start)
      (insert (format "function %s" (imalison:js-get-left-name))))))
