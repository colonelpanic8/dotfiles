;; Elpaca Installer -*- lexical-binding: t; -*-
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-legacy-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))

(defun elpaca-installer--repo-installer-version (repo)
  "Return the installer version expected by elpaca checkout at REPO."
  (let ((elpaca-el (expand-file-name "elpaca.el" repo)))
    (when (file-exists-p elpaca-el)
      (with-temp-buffer
        (insert-file-contents elpaca-el)
        (when (re-search-forward
               "(= elpaca-installer-version \\([0-9.]+\\))"
               nil t)
          (match-string 1))))))

(defun elpaca-installer--build-source-root (build)
  "Infer the source directory backing BUILD from its symlinked files."
  (let ((roots (delete-dups
                (delq nil
                      (mapcar
                       (lambda (dir)
                         (when (file-exists-p dir)
                           (file-name-as-directory (file-truename dir))))
                       (list elpaca-sources-directory
                             elpaca-legacy-repos-directory))))))
    (catch 'repo
      (dolist (entry (directory-files build t directory-files-no-dot-files-regexp))
        (when-let* ((target (file-symlink-p entry))
                    (truename (ignore-errors (file-truename entry)))
                    (source-root (and truename
                                      (directory-file-name
                                       (file-name-directory truename)))))
          (dolist (root roots)
            (when (string-prefix-p root truename)
              (when (file-directory-p source-root)
                (throw 'repo source-root))))))
      nil)))

(defun elpaca-installer--repair-build-source-layout ()
  "Repair or prune builds whose expected source directory is missing."
  (when (and (file-directory-p elpaca-builds-directory)
             (file-directory-p elpaca-sources-directory))
    (dolist (build (directory-files elpaca-builds-directory t directory-files-no-dot-files-regexp))
      (when (file-directory-p build)
        (let* ((name (file-name-nondirectory (directory-file-name build)))
               (source (expand-file-name name elpaca-sources-directory))
               (desired-source (when-let ((root (elpaca-installer--build-source-root build)))
                                 (directory-file-name root)))
               (current-source (when (or (file-exists-p source)
                                         (file-symlink-p source))
                                 (ignore-errors
                                   (directory-file-name (file-truename source))))))
          (cond
           ((and desired-source
                 current-source
                 (equal current-source desired-source))
            nil)
           ((and (file-directory-p source)
                 (not (file-symlink-p source)))
            nil)
           ((file-symlink-p source)
            (delete-file source)
            (if desired-source
                (make-symbolic-link desired-source
                                    (directory-file-name source))
              (delete-directory build 'recursive)))
           ((file-exists-p source) nil)
           (desired-source
            (make-symbolic-link desired-source
                                (directory-file-name source)))
           (t
            (delete-directory build 'recursive))))))))
;; Elpaca now expects package sources under `sources/`. Preserve older local
;; installs that still use `repos/` so startup can recover without recloning.
(when (and (file-directory-p elpaca-legacy-repos-directory)
           (not (file-exists-p elpaca-sources-directory)))
  (rename-file (directory-file-name elpaca-legacy-repos-directory)
               (directory-file-name elpaca-sources-directory)))
(when (and (file-directory-p elpaca-sources-directory)
           (not (file-exists-p elpaca-legacy-repos-directory)))
  (make-symbolic-link (directory-file-name elpaca-sources-directory)
                      (directory-file-name elpaca-legacy-repos-directory)))
(elpaca-installer--repair-build-source-layout)
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (autoloads (expand-file-name "elpaca-autoloads" repo))
       (order (cdr elpaca-order))
       (default-directory repo))
  ;; Older elpaca checkouts can no longer bootstrap the current installer.
  ;; Reset only elpaca's own repo/build so startup can self-heal.
  (when-let ((repo-version (elpaca-installer--repo-installer-version repo))
             ((not (equal repo-version (format "%s" elpaca-installer-version)))))
    (when (file-directory-p build)
      (delete-directory build 'recursive))
    (when (file-directory-p elpaca-cache-directory)
      (delete-directory elpaca-cache-directory 'recursive))
    (when (file-directory-p repo)
      (delete-directory repo 'recursive)))
  (add-to-list 'load-path repo)
  (when (file-exists-p build)
    (add-to-list 'load-path build))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load autoloads))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
