(require 'eieio)
(require 'projectile)
(require 'contextual-value-manager)


(defclass term-projectile-identifier-manager (cvm-identifier-manager eieio-singleton)
  ((identifier-to-value :initarg :identifier-to-value :initform nil)))

(defmethod cvm-get-identifier ((manager term-projectile-identifier-manager))
  (when (projectile-project-p)
    (intern (projectile-project-root))))

(defmethod cvm-get-value ((manager term-projectile-identifier-manager) identifier)
  (let ((buffer (plist-get (oref manager :identifier-to-value) identifier)))
    (when (and buffer (buffer-live-p buffer))
      buffer)))

(defmethod cvm-set-value ((manager term-projectile-identifier-manager) identifier value)
  (oset manager :identifier-to-value (plist-put (oref manager :identifier-to-value) identifier value)))

(defclass term-builder (cvm-builder eieio-singleton) nil)

(defmethod cvm-build ((builder term-builder) identifier)
  (let* ((term-name (build-term-name builder identifier))
         (program (getenv "SHELL"))
         (directory (if identifier (symbol-name identifier) "~"))
         (buffer (get-buffer (term-ansi-make-term term-name program))))
    (with-current-buffer buffer
      (term-mode)
      (term-char-mode)
      (let (term-escape-char)
        (term-set-escape-char ?\C-x)))
    buffer))

(defmethod build-term-name ((builder term-builder) identifier)
  (format  "term: %s" (symbol-name identifier)))

(defclass projectile-term-manager (cvm-contextual-value-manager eieio-singleton) nil)

(make-instance projectile-term-manager
               :builder (make-instance term-builder)
               :identifier-manager (make-instance term-projectile-identifier-manager))

(defun term-projectile ()
  (interactive)
  (switch-to-buffer (cvm-contextual-value (make-instance projectile-term-manager))))

(switch-to-buffer (cvm-contextual-value (make-instance projectile-term-manager)))

(cvm-get-identifier (make-instance term-projectile-identifier-manager))

(provide 'term-projectile)
