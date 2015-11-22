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
  (let ((markers (multi-line-get-markers
                  (multi-line-get-enter-strategy multi-line-config)
                  (multi-line-get-find-strategy multi-line-config))))
    (message "%d" markers)))

(defclass bars-at-markers () nil)

(defmethod multi-line-respace ((respacer bars-at-markers) index markers)
  (insert "|"))

(defun multiline-test-bars()
  (interactive)
  (multi-line-adjust-whitespace (make-instance bars-at-markers)))
