;;; dbus.el --- Disabled D-Bus shim -*- lexical-binding: t; -*-

;;; Commentary:
;; This file intentionally shadows Emacs' built-in net/dbus.el.  Loading the
;; built-in library initializes D-Bus connections at top level, which is noisy
;; and fragile in this configuration.

;;; Code:

(setq features (delq 'dbusbind features))
(setq dbus-compiled-version nil
      dbus-runtime-version nil)

(unless (boundp 'dbus-error)
  (define-error 'dbus-error "D-Bus disabled in this Emacs config"))

(defvar dbus-debug nil)
(defvar dbus-event-error-functions nil)
(defvar dbus-registered-objects-table (make-hash-table :test #'equal))

(defconst dbus-service-dbus "org.freedesktop.DBus")
(defconst dbus-path-dbus "/org/freedesktop/DBus")
(defconst dbus-path-local "/org/freedesktop/DBus/Local")
(defconst dbus-interface-dbus "org.freedesktop.DBus")
(defconst dbus-interface-peer "org.freedesktop.DBus.Peer")
(defconst dbus-interface-introspectable "org.freedesktop.DBus.Introspectable")
(defconst dbus-interface-properties "org.freedesktop.DBus.Properties")
(defconst dbus-interface-objectmanager "org.freedesktop.DBus.ObjectManager")
(defconst dbus-interface-monitoring "org.freedesktop.DBus.Monitoring")
(defconst dbus-interface-local "org.freedesktop.DBus.Local")
(defconst dbus-service-emacs "org.gnu.Emacs")
(defconst dbus-path-emacs "/org/gnu/Emacs")
(defconst dbus-interface-emacs "org.gnu.Emacs")

(defconst dbus-error-dbus "org.freedesktop.DBus.Error")
(defconst dbus-error-failed "org.freedesktop.DBus.Error.Failed")
(defconst dbus-error-service-unknown "org.freedesktop.DBus.Error.ServiceUnknown")

(defmacro dbus-ignore-errors (&rest body)
  "Execute BODY, suppressing `dbus-error' unless `dbus-debug' is non-nil."
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     (dbus-error (when dbus-debug (signal (car err) (cdr err))))))

(defun imalison:dbus-disabled (&rest _args)
  "Signal that D-Bus is intentionally unavailable in this config."
  (signal 'dbus-error '("D-Bus disabled in this Emacs config")))

(dolist (function
         '(dbus-call-method
           dbus-call-method-asynchronously
           dbus-send-signal
           dbus-method-return-internal
           dbus-method-error-internal
           dbus-register-service
           dbus-unregister-service
           dbus-register-signal
           dbus-register-method
           dbus-unregister-object
           dbus-register-property
           dbus-register-monitor
           dbus-init-bus
           dbus-ping
           dbus-introspect
           dbus-introspect-xml
           dbus-get-property
           dbus-set-property
           dbus-get-all-properties
           dbus-get-all-managed-objects))
  (defalias function #'imalison:dbus-disabled))

(defun dbus-list-activatable-names (&optional _bus) nil)
(defun dbus-list-names (&optional _bus) nil)
(defun dbus-list-known-names (&optional _bus) nil)
(defun dbus-list-queued-owners (&rest _args) nil)
(defun dbus-get-name-owner (&rest _args) nil)
(defun dbus-list-hash-table () nil)

(defun dbus-event-bus-name (_event) nil)
(defun dbus-event-message-type (_event) nil)
(defun dbus-event-serial-number (_event) nil)
(defun dbus-event-service-name (_event) nil)
(defun dbus-event-destination-name (_event) nil)
(defun dbus-event-path-name (_event) nil)
(defun dbus-event-interface-name (_event) nil)
(defun dbus-event-member-name (_event) nil)
(defun dbus-event-handler (_event) nil)
(defun dbus-event-arguments (_event) nil)

(provide 'dbus)

;;; dbus.el ends here
