;;; gitolite-clone.el --- Clone gitolite repositories from a completing list -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: gitolite clone git
;; URL: https://github.com/IvanMalison/gitolite-cloone
;; Version: 0.1.0
;; Package-Requires: ((dash "2.10.0") (s "1.9.0") (pcache "0.3.1") (emacs "24"))

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

(defvar gitolite-clone-pcache-repository (pcache-repository "gitolite-clone"))
(defcustom gitolite-clone-username "gitolite"
  "The username that will be used to connect to gitolite by gitoline-clone."
  :group 'gitolite-clone)
(defcustom gitolite-clone-host ""
  "The gitolite host that will be connected to by default by gitoline-clone."
  :group 'gitolite-clone)
(defcustom gitolite-clone-ttl (* 60 60 24 3)
  "The default pcache ttl that will be used for caching repository results from gitolite."
  :group 'gitolite-clone)
(defcustom gitolite-clone-base-path "~"
  "The base path to which `gitolite-clone-default-determine-target' will clone repositories"
  :group 'gitolite-clone)
(defvar gitolite-clone-determine-target 'gitolite-clone-default-determine-target)
(defvar gitolite-clone-action 'gitolite-clone-default-action)

(defun gitolite-clone-default-determine-target (username host repo-name)
  "A sensible default for determining the path to which repositories are cloned.

USERNAME and HOST are ignored. The folder will reside in
`gitolite-clone-base-path' and its name will be the text
following the final '/' in REPO-NAME."
  (format "%s/%s" gitolite-clone-base-path (-last-item (s-split "/" repo-name))))

(defun gitolite-clone-default-action (username host repo-name target)
  "Open dired at on the newly cloned repository.

USERNAME, HOST, and REPO-NAME are ignored. Dired will be opened at TARGET."
  (dired target))

(defun gitolite-clone-info-command (username host)
  "Generate command to retrieve the list of repositories from gitolite.

USERNAME is the username used on the gitolite server and HOST is
the hostname of the gitolite server."
  (format "ssh %s@%s info" (shell-quote-argument username) (shell-quote-argument host)))

(defun gitolite-clone-get-projects-list-string (username host)
  "Make a call to the gitolite server to retrieve list of repos.
a custom username can be provided with USERNAME and custom host
can be probided with HOST."
  (shell-command-to-string (gitolite-clone-info-command username host)))

(defun gitolite-clone-parse-projects-list-string (projects-list-string)
    "Get all lines that correspond to an actual repository from PROJECTS-LIST-STRING."
  (cl-loop for matches in (s-match-strings-all "^ \[RWC ]*	\\(\[^ *\]*\\)$" projects-list-string)
           when (not (s-contains? "*" (nth 1 matches)))
           collect (nth 1 matches)))

(defun gitolite-clone-get-projects (&optional username host force-refresh)
  "Retrieve and parse the list of projects available from gitolite.

USERNAME is the username used on the gitolite server and HOST is
the hostname of the gitolite server. The retrieval is cached an
will only occur if the result has not already been stored.
FORCE-REFRESH makes it so that the cache is ignored when non nil."
  (unless username (setq username gitolite-clone-username))
  (unless host (setq host gitolite-clone-host))
  (let ((result-key (intern (format "%s@%s" username host))))
    (unless (and (pcache-has gitolite-clone-pcache-repository result-key) (not force-refresh))
      (pcache-put gitolite-clone-pcache-repository result-key
                  (gitolite-clone-parse-projects-list-string
                   (gitolite-clone-get-projects-list-string username host)) gitolite-clone-ttl))
    (pcache-get gitolite-clone-pcache-repository result-key)))

(defun gitolite-clone-select-repository (&optional username host)
  "Pick a repository from the one available from the gitolite server.

USERNAME is the username used on the gitolite server and HOST is
the hostname of the gitolite server."
  (completing-read "Choose a repository:" (gitolite-clone-get-projects username host)))

;;;###autoload
(defun gitolite-clone (&optional username host determine-target action)
  "Clone a gitolite repo to be selected by `completing-read'.

USERNAME and HOST will be used to determine how to talk to
gitolite using ssh.  They default to `gitolite-clone-username' and
`gitolite-clone-host' respectively.

DETERMINE-TARGET is a function with signature identical to that
of `gitolite-clone-default-determine-target'.  It will determine
the path to which the repository should be cloned, and it
defaults to `gitolite-clone-default-determine-target'.

ACTION is a function that will be executed once the repository
has been cloned.  It's signature should be that of
`gitolite-clone-default-action', which is also the default value
for this argument."
  (interactive)
  (unless username (setq username gitolite-clone-username))
  (unless host (setq host gitolite-clone-host))
  (unless determine-target (setq determine-target gitolite-clone-determine-target))
  (unless action (setq action gitolite-clone-action))
  (let* ((repository (gitolite-clone-select-repository))
         (target (funcall determine-target username host repository))
         (git-command (format "git clone %s@%s:%s %s" (shell-quote-argument username)
                              (shell-quote-argument host) (shell-quote-argument repository)
                              (shell-quote-argument target))))
    (unless (file-exists-p target)
      (shell-command git-command))
    (when (file-exists-p target)
      (funcall action username host repository target))))

(provide 'gitolite-clone)
;;; gitolite-clone.el ends here
