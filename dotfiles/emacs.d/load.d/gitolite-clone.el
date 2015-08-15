;;; gitolite-clone.el --- Clone gitolite repositories from a completing list -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: gitolite clone git
;; URL: https://github.com/IvanMalison/gitolite-cloone
;; Version: 0.1.0
;; Package-Requires: ((dash "2.10.0") (s "1.9.0") (pcache "0.3.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; gitolite-clone provides a cached completing read of the
;; repositories available on a gitolite server and the ability to set
;; rules for how repositories should be cloned.

;;; Code:

(require 'dash)
(require 'pcache)
(require 's)

(defvar gitolite-clone:pcache-repository (pcache-repository "gitolite-clone"))
(defvar gitolite-clone:username "gitolite")
(defvar gitolite-clone:host)
(defvar gitolite-clone:ttl (* 60 60 24 3)) ;; 3 day ttl by default
(defvar gitolite-clone:base-path "~")
(defvar gitolite-clone:determine-target 'gitolite-clone:default-determine-target)
(defvar gitolite-clone:action 'gitolite-clone:default-action)

(defun gitolite-clone:default-determine-target (username host repo-name)
  (format "%s/%s" gitolite-clone:base-path (-last-item (s-split "/" repo-name))))

(defun gitolite-clone:default-action (username host repo-name target)
  (dired target))

(defun gitolite-clone:info-command (username host)
  (format "ssh %s@%s info" username host))

(defun gitolite-clone:get-projects-list-string (username host)
  (shell-command-to-string (gitolite-clone:info-command username host)))

(defun gitolite-clone:repo-matches (projects-list-string)
  (s-match-strings-all "^ \[RWC ]*	\\(\[^ *\]*\\)$" projects-list-string))

(defun gitolite-clone:parse-projects-list-string (projects-list-string)
  (cl-loop for matches in (gitolite-clone:repo-matches projects-list-string)
           when (not (s-contains? "*" (nth 1 matches)))
           collect (nth 1 matches)))

(defun gitolite-clone:get-projects (&optional username host force-refresh)
  (unless username (setq username gitolite-clone:username))
  (unless host (setq host gitolite-clone:host))
  (let ((result-key (intern (format "%s@%s" username host))))
    (unless (and (pcache-has gitolite-clone:pcache-repository result-key) (not force-refresh))
      (pcache-put gitolite-clone:pcache-repository result-key
                  (gitolite-clone:parse-projects-list-string
                   (gitolite-clone:get-projects-list-string username host)) gitolite-clone:ttl))
    (pcache-get gitolite-clone:pcache-repository result-key)))

(defun gitolite-clone:select-repository (&optional username host)
  (completing-read "Choose a repository:" (gitolite-clone:get-projects username host)))

;;;###autoload
(defun gitolite-clone:clone-repo (&optional username host determine-target action)
  (interactive)
  (unless username (setq username gitolite-clone:username))
  (unless host (setq host gitolite-clone:host))
  (unless determine-target (setq determine-target gitolite-clone:determine-target))
  (unless action (setq action gitolite-clone:action))
  (let* ((repository (gitolite-clone:select-repository))
         (target (funcall determine-target username host repository))
         (git-command (format "git clone %s@%s:%s %s" username host repository target)))
    (unless (file-exists-p target)
      (shell-command git-command))
    (when (file-exists-p target)
      (funcall action username host repository target))))

(provide 'gitolite-clone)
;;; gitolite-clone.el ends here
