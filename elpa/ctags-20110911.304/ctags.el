;;; ctags.el --- Exuberant Ctags utilities for Emacs

;; Copyright  (C)  2011 Free Software Foundation, Inc.

;; Version: 20110911.304
;; X-Original-Version: 1.1.1
;; Keywords: tags ctags etags
;; Author: Guilherme M. Gondim <semente@taurinus.org>
;; Maintainer: Guilherme M. Gondim <semente@taurinus.org>
;; URL: https://bitbucket.org/semente/ctags.el

;; This file is NOT part of GNU Emacs.

;; ctags.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ctags.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ctags.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configuration example:

;; (setq tags-revert-without-query t)
;; (global-set-key (kbd "<f7>") 'ctags-create-or-update-tags-table)

;; Then just press <f7> to update or create your TAGS file. That function look
;; for a file TAGS in the current and its parent directories, if a TAG file is
;; not found it ask you where create a new one.

;; Optionally you can use the function `ctags-search', a little wrapper for
;; `tags-search' that provides a default input like in `find-tag', to search
;; through all files listed in tags table.

;; Also, if you prefer, you can override the key binding M-. for `find-tag' to
;; use `ctags-search':

;; (global-set-key (kbd "M-.")  'ctags-search)

;;; Installation:

;; Add this to your Emacs configuration:

;; (add-to-list 'load-path "/folder/containing/file")
;; (require 'ctags)

;; Alternatively, you can install it using the Marmalade ELPA repository.

;;; Code:

(defvar ctags-command "/usr/bin/ctags -e -R ")

(defun ctags ()
  (call-process-shell-command ctags-command nil "*Ctags*"))


(defun ctags-find-tags-file ()
  "Recursively searches each parent directory for a file named
TAGS and returns the path to that file or nil if a tags file is
not found or if the buffer is not visiting a file."
  (progn
    (defun find-tags-file-r (path)
      "Find the tags file from current to the parent directories."
      (let* ((parent-directory (file-name-directory (directory-file-name path)))
             (tags-file-name (concat (file-name-as-directory path) "TAGS")))
        (cond
         ((file-exists-p tags-file-name) (throw 'found tags-file-name))
         ((string= "/TAGS" tags-file-name) nil)
         (t (find-tags-file-r parent-directory)))))

    (if (buffer-file-name)
        (catch 'found
          (find-tags-file-r (file-name-directory buffer-file-name)))
      nil)))

(defun ctags-set-tags-file ()
  "Uses `ctags-find-tags-file' to find a TAGS file. If found,
set 'tags-file-name' with its path or set as nil."
  (setq-default tags-file-name (ctags-find-tags-file)))

(defun ctags-create-tags-table ()
  (interactive)
  (let* ((current-directory default-directory)
         (top-directory (read-directory-name
                         "Top of source tree: " default-directory))
         (file-name (concat (file-name-as-directory top-directory) "TAGS")))
    (cd top-directory)
    (if (not (= 0 (ctags)))
        (message "Error creating %s!" file-name)
      (setq-default tags-file-name file-name)
      (message "Table %s created and configured." tags-file-name))
    (cd current-directory)))

(defun ctags-update-tags-table ()
  (interactive)
  (let ((current-directory default-directory))
    (if (not tags-file-name)
        (message "Tags table not configured.")
      (cd (file-name-directory tags-file-name))
      (if (not (= 0 (ctags)))
          (message "Error updating %s!" tags-file-name)
        (message "Table %s updated." tags-file-name))
      (cd current-directory))))

(defun ctags-create-or-update-tags-table ()
  "Create or update a tags table with `ctags-command'."
  (interactive)
  (if (not (ctags-set-tags-file))
      (ctags-create-tags-table)
    (ctags-update-tags-table)))


(defun ctags-search ()
  "A wrapper for `tags-search' that provide a default input."
  (interactive)
  (let* ((symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (input (read-from-minibuffer
                 (if (symbol-at-point)
                     (concat "Tags search (default " default "): ")
                   "Tags search (regexp): "))))
    (if (and (symbol-at-point) (string= input ""))
        (tags-search default)
      (if (string= input "")
          (message "You must provide a regexp.")
        (tags-search input)))))


(provide 'ctags)
;;; ctags.el ends here
