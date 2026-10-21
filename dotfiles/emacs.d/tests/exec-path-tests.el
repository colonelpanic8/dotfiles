;;; exec-path-tests.el --- GUI executable lookup tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'imalison-exec-path)

(ert-deftest exec-path-adds-managed-tools-to-gui-environment ()
  (let* ((directory (make-temp-file "exec-path-test-" t))
         (tool (expand-file-name "direnv" directory))
         (process-environment (copy-sequence process-environment))
         (exec-path '("/usr/bin" "/bin")))
    (unwind-protect
        (progn
          (with-temp-file tool (insert "#!/bin/sh\nexit 0\n"))
          (set-file-modes tool #o755)
          (setenv "PATH" "/usr/bin:/bin")
          (imalison:initialize-exec-path (list directory))
          (should (equal (executable-find "direnv") tool))
          (should (zerop (call-process "sh" nil nil nil "-c" "direnv")))
          (let ((path (getenv "PATH")) (search-path exec-path))
            (imalison:initialize-exec-path (list directory))
            (should (equal (getenv "PATH") path))
            (should (equal exec-path search-path))))
      (delete-directory directory t))))

(ert-deftest exec-path-ignores-uninstalled-profiles ()
  (let* ((directory (make-temp-file "exec-path-test-" t))
         (missing (expand-file-name "missing" directory))
         (process-environment (copy-sequence process-environment))
         (exec-path '("/usr/bin" "/bin")))
    (unwind-protect
        (progn
          (setenv "PATH" "/usr/bin:/bin")
          (imalison:initialize-exec-path (list missing))
          (should (equal exec-path '("/usr/bin" "/bin")))
          (should (equal (getenv "PATH") "/usr/bin:/bin")))
      (delete-directory directory))))

;;; exec-path-tests.el ends here
