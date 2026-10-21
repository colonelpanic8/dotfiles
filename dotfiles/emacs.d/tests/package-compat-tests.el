;;; package-compat-tests.el --- Package build regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'imalison-package-compat)

(ert-deftest package-compat-preserves-conditional-semantics ()
  (dolist (form '((if-let (x 7) x 'fallback)
                  (if-let ((x nil)) x 'fallback)
                  (when-let (x 7) (+ x 1))
                  (when-let (x nil) (error "Unreachable"))
                  (if-let ((x 7) (y (+ x 1))) y 'fallback)
                  (if-let ((x (if-let (y 7) y nil))) x nil)))
    (let ((file (make-temp-file "package-compat-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file file (prin1 form (current-buffer)))
            (should (imalison:package-compat-file file))
            (with-temp-buffer
              (insert-file-contents file)
              (let ((patched (read (current-buffer))))
                (should (equal (eval form t) (eval patched t)))
                (should (equal (macroexpand-all form)
                               (macroexpand-all patched)))))
            (let ((mtime (file-attribute-modification-time (file-attributes file))))
              (should-not (imalison:package-compat-file file))
              (should (equal mtime (file-attribute-modification-time
                                   (file-attributes file))))))
        (delete-file file)))))

(ert-deftest package-compat-keeps-sources-clean ()
  (let* ((dir (make-temp-file "package-compat-" t))
         (source (expand-file-name "source.el" dir))
         (build (expand-file-name "build.el" dir))
         (text "; (if-let (x 1) x)\n\"(when-let (x 1) x)\"\n(if-let (x 7) x)\n"))
    (unwind-protect
        (progn
          (with-temp-file source (insert text))
          (make-symbolic-link source build)
          (with-temp-file (concat build "c") (insert "stale bytecode"))
          (should (imalison:package-compat-file build))
          (should-not (file-symlink-p build))
          (should-not (file-exists-p (concat build "c")))
          (with-temp-buffer
            (insert-file-contents source)
            (should (equal (buffer-string) text)))
          (with-temp-buffer
            (insert-file-contents build)
            (should (equal (buffer-string)
                           (concat "; (if-let (x 1) x)\n"
                                   "\"(when-let (x 1) x)\"\n"
                                   "(if-let* ((x 7)) x)\n")))))
      (delete-directory dir t))))

(ert-deftest package-compat-runs-before-compilation-on-rebuild ()
  (dolist (id '(smartparens org-fc unrelated))
    (let* ((e (elpaca<--create :id id :recipe nil))
           (steps (elpaca-build-steps e :rebuild)))
      (if (eq id 'unrelated)
          (should-not (memq 'imalison:package-compat-build steps))
        (should (eq (cadr (memq 'elpaca-build-link steps))
                    'imalison:package-compat-build))
        (should (memq 'elpaca-build-compile
                      (memq 'imalison:package-compat-build steps)))))))

;;; package-compat-tests.el ends here
