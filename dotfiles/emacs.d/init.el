(require 'package)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(defun ensure-packages-installed (packages)
  (unless package-archive-contents
    (package-refresh-contents))
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         package
       (progn (message (format "Installing package %s." package))
              (package-install package))))
   packages))

(package-initialize)
(ensure-packages-installed '(org-plus-contrib))

(org-babel-load-file
 (concat (file-name-directory load-file-name) "config.org") t)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
