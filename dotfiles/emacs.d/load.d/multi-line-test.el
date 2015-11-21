(defun test-multi-line-enter ()
  (interactive)
  (multi-line-enter
   (make-instance multi-line-forward-sexp-enter-strategy)))

(defun test-multi-line-find-next ()
  (interactive)
  (message "%s" (multi-line-find-next
                 (make-instance multi-line-forward-sexp-find-strategy))))

(defun test-multi-line-get-markers ()
  (interactive)
  (message "%d" (length (multi-line-get-markers
                    (make-instance multi-line-forward-sexp-enter-strategy)
                    (make-instance multi-line-forward-sexp-find-strategy)))))
