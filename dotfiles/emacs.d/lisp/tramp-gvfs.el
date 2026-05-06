;;; tramp-gvfs.el --- Disabled TRAMP GVFS backend -*- lexical-binding: t; -*-

;;; Commentary:
;; This file intentionally shadows Emacs' built-in net/tramp-gvfs.el.  The real
;; backend depends on D-Bus and GVFS desktop services; this config uses ordinary
;; TRAMP methods instead.

;;; Code:

(require 'tramp)

(defconst tramp-gvfs-enabled nil
  "Non-nil when the disabled GVFS backend is available.")

(defvar tramp-gvfs-methods nil
  "Disabled list of TRAMP methods handled through GVFS.")
(setq tramp-gvfs-methods nil)

(defconst tramp-goa-methods nil
  "Disabled list of GNOME Online Accounts TRAMP methods.")

(defvar tramp-media-methods nil
  "Disabled list of media-device TRAMP methods.")
(setq tramp-media-methods nil)

(defvar tramp-gvfs-file-name-handler-alist nil
  "Disabled GVFS file name handler alist.")

(defun tramp-gvfs-file-name-p (_filename)
  "Return nil because the GVFS backend is disabled."
  nil)

(defun tramp-gvfs-file-name-handler (operation &rest args)
  "Signal an unsupported OPERATION for the disabled GVFS backend."
  (signal 'file-error
          (list "TRAMP GVFS backend disabled in this Emacs config"
                operation args)))

(provide 'tramp-gvfs)

;;; tramp-gvfs.el ends here
