(require 'pcache)

(defvar gitolite:pcache-repository (pcache-repository "gitolite"))
(defvar gitolite:username "gitolite")
(defvar gitolite:host)
(defvar gitolite:ttl (* 60 60 24 3)) ;; 3 day ttl by default
(defvar gitolite:base-path "~")
(defvar gitolite:determine-target 'gitolite:default-determine-target)
(defvar gitolite:action 'gitolite:default-action)

(defun gitolite:default-determine-target (username host repo-name)
  (format "%s/%s" gitolite:base-path (-last-item (s-split "/" repo-name))))

(defun gitolite:default-action (username host repo-name target)
  (dired target))

(defun gitolite:info-command (username host)
  (format "ssh %s@%s info" username host))

(defun gitolite:get-projects-list-string (username host)
  (shell-command-to-string (gitolite:info-command username host)))

(defun gitolite:repo-matches (projects-list-string)
  (s-match-strings-all "^ \[RWC ]*	\\(\[^ *\]*\\)$" projects-list-string))

(defun gitolite:parse-projects-list-string (projects-list-string)
  (cl-loop for matches in (gitolite:repo-matches projects-list-string)
           when (not (s-contains? "*" (nth 1 matches)))
           collect (nth 1 matches)))

(defun gitolite:get-projects (&optional username host force-refresh)
  (unless username (setq username gitolite:username))
  (unless host (setq host gitolite:host))
  (let ((result-key (intern (format "%s@%s" username host))))
    (unless (and (pcache-has gitolite:pcache-repository result-key) (not force-refresh))
      (pcache-put gitolite:pcache-repository result-key
                  (gitolite:parse-projects-list-string
                   (gitolite:get-projects-list-string username host)) gitolite:ttl))
    (message "%s" result-key)
    (pcache-get gitolite:pcache-repository result-key)))

(defun gitolite:select-repository (&optional username host)
  (helm :sources (helm-build-sync-source "Choose a repository:"
                   :candidates (gitolite:get-projects username host))))

(defun gitolite:clone-repo (&optional username host determine-target action)
  (interactive)
  (unless username (setq username gitolite:username))
  (unless host (setq host gitolite:host))
  (unless determine-target (setq determine-target gitolite:determine-target))
  (unless action (setq action gitolite:action))
  (let* ((repository (gitolite:select-repository))
         (target (funcall determine-target username host repository))
         (git-command (format "git clone %s@%s:%s %s" username host repository target)))
    (unless (file-exists-p target)
      (shell-command git-command))
    (when (file-exists-p target)
      (funcall action username host repository target))))
