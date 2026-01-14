;;; org-api.el --- org-agenda-api server configuration -*- lexical-binding: t; -*-

;; Load main emacs config to get org settings, custom commands, etc.
(let ((current-dir (if load-file-name (file-name-directory load-file-name) "~/.emacs.d/")))
  (load (expand-file-name "init.el" current-dir)))

;; Ensure dependencies are loaded
(require 'simple-httpd)
(require 'org-agenda-api)

;; Configure capture templates for the API
;; These use the org-agenda-api format with :prompts for API parameters
(setq org-agenda-api-capture-templates
      `(("gtd-todo"
         :name "GTD Todo"
         :template ("t" "Todo" entry (file "~/org/inbox.org")
                    (function (lambda ()
                                (imalison:make-org-todo-template :content "%^{Title}")))
                    :immediate-finish t)
         :prompts (("Title" :type string :required t)))
        ("scheduled-todo"
         :name "Scheduled Todo"
         :template ("s" "Scheduled" entry (file "~/org/inbox.org")
                    "* TODO %^{Title}\nSCHEDULED: %^{When}t\n"
                    :immediate-finish t)
         :prompts (("Title" :type string :required t)
                   ("When" :type date :required t)))
        ("tagged-todo"
         :name "Tagged Todo"
         :template ("g" "Tagged" entry (file "~/org/inbox.org")
                    "* TODO %^{Title} %^{Tags}g\n"
                    :immediate-finish t)
         :prompts (("Title" :type string :required t)
                   ("Tags" :type tags :required nil)))))

;; Configure the server
(setq org-agenda-api-port 2025)
(setq org-agenda-api-inbox-file "~/org/inbox.org")

;; Start the server
(message "Starting org-agenda-api server on port %d" org-agenda-api-port)
(org-agenda-api-start)
(message "org-agenda-api server started")
