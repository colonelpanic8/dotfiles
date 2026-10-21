;;; imalison-exec-path.el --- Executables for GUI Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Finder and launchd start Emacs without the Nix shell environment.  Make
;; managed tools available before Elpaca or any package starts a subprocess.

;;; Code:

(require 'seq)

(defun imalison:initialize-exec-path (&optional directories)
  "Add existing managed DIRECTORIES to both `exec-path' and PATH.
Default to the user's Nix profiles and the active system installation."
  (let ((directories
         (seq-filter
          #'file-directory-p
          (or directories
              (list (expand-file-name "~/.local/bin")
                    (concat "/etc/profiles/per-user/" (user-login-name) "/bin")
                    (expand-file-name "~/.nix-profile/bin")
                    "/run/current-system/sw/bin"
                    "/nix/var/nix/profiles/default/bin")))))
    (setq exec-path (delete-dups (append directories exec-path)))
    (setenv "PATH"
            (mapconcat #'identity
                       (delete-dups
                        (append directories
                                (split-string (or (getenv "PATH") "")
                                              path-separator t)))
                       path-separator))))

(provide 'imalison-exec-path)
;;; imalison-exec-path.el ends here
