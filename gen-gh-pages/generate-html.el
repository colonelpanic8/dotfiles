(package-initialize)

(require 'ox-html)
(require 'htmlize)

(defmacro do-nothing (name)
  `(defun ,name (&rest args) nil))

(do-nothing imalison:add-to-org-agenda-files)
(do-nothing imalison:disable-linum-mode)

(setq emacs-dir
      (expand-file-name "../dotfiles/emacs.d/"
                        (file-name-directory load-file-name)))

(setq readme-src (concat emacs-dir "README.org"))

(require 'use-package)
(require 's)
(org-babel-tangle-file readme-src)

(load-file (concat emacs-dir "org-config.el"))

(defun export-target (target)
  (with-current-buffer (find-file-noselect target)
    (let ((org-export-headline-levels 10))
      (org-html-export-to-html))))

(setq org-html-postamble nil)
(setq org-html-htmlize-output-type 'css)
(setq org-confirm-babel-evaluate nil)

(defun add-faces-css (exporter)
  "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
  (setq
   org-html-head-extra
   (concat
    org-html-head-extra
    (format "<style type=\"text/css\">\n%s</style>\n"
            (with-temp-buffer
              (insert-file-contents
               (concat (file-name-directory load-file-name) "faces.css"))
              (buffer-string))))))

(add-hook 'org-export-before-processing-hook 'add-faces-css)
(remove-hook 'org-export-before-processing-hook 'imalison:org-inline-css-hook)

(message (export-target readme-src))
