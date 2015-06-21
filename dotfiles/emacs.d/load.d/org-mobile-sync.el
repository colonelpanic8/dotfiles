(defvar imalison-org-mobile-sync-timer nil)
(defvar imalison-org-mobile-sync-secs 60)
(defvar imalison-org-mobile-sync:unsaved-changes-count 0)
(defvar imalison-org-mobile-sync:unsaved-changes-min-sync 1)

(defun imalison-org-mobile-sync-pull-and-push ()
  (suppress-messages (org-mobile-pull))
  (imalison-org-mobile-sync)
  (when (fboundp 'sauron-add-event)
    (sauron-add-event 'me 1 "Called org-mobile-pull and org-mobile-push")))

(defun imalison-org-mobile-sync-start ()
  "Start automated `org-mobile-push'"
  (interactive)
  (setq imalison-org-mobile-sync-timer
        (run-with-idle-timer imalison-org-mobile-sync-secs t
                             'imalison-org-mobile-sync-pull-and-push)))

(defun imalison-org-mobile-sync-stop ()
  "Stop automated `org-mobile-push'"
  (interactive)
  (cancel-timer imalison-org-mobile-sync-timer))

(defun imalison-org-mobile-sync (&optional force)
  (interactive)
  (when (or force (>= imalison-org-mobile-sync:unsaved-changes-count
                      imalison-org-mobile-sync:unsaved-changes-min-sync))
    (org-mobile-push)
    (setq imalison-org-mobile-sync:unsaved-changes-count 0)))

(add-hook 'after-save-hook (lambda ()
                             (when (member (file-truename (buffer-file-name))
                                             (mapcar 'file-truename org-agenda-files))
                               (setq imalison-org-mobile-sync:unsaved-changes-count
                                     (+ imalison-org-mobile-sync:unsaved-changes-count 1)))))

(when nil ;; disabled for now.
    (if (and (boundp 'file-notify--library) file-notify--library)
    (use-package org-mobile-sync :ensure t :config (org-mobile-sync-mode 1))
  (imalison-org-mobile-sync-start)))

(provide 'org-mobile-sync)
