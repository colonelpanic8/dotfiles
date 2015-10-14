(require 'eieio)

(defclass cvm-contextual-value-manager ()
  ((identifier-manager :initarg :identifier-manager)
   (builder :initarg :builder)))

(defmethod cvm-contextual-value ((cvm cvm-contextual-value-manager))
  (let ((identifier (cvm-get-identifier (oref cvm :identifier-manager))))
    (or (cvm-get-value (oref cvm :identifier-manager) identifier)
        (cvm-build-and-save cvm identifier))))

(defmethod cvm-build-and-save ((cvm cvm-contextual-value-manager) identifier)
  (let ((value (cvm-build (oref cvm :builder) identifier)))
    (cvm-set-value (oref cvm :identifier-manager) identifier value)
    value))

(defclass cvm-identifier-manager () nil
  :abstract t)

(defmethod cvm-get-identifier ((generator cvm-identifier-manager))
  nil)

(defmethod cvm-get-value ((generator cvm-identifier-manager) identifier)
  nil)

(defmethod cvm-set-value ((generator cvm-identifier-manager) identifier value)
  nil)

(defclass cvm-builder () nil
  :abstract t)

(defmethod cvm-build ((builder cvm-builder) identifier)
  nil)

(provide 'contextual-value-manager)
