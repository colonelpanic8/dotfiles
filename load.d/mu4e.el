(use-package s :ensure t)
(add-to-list 'load-path (s-trim (shell-command-to-string "mu4e_directory")))

(use-package mu4e
  :config
  (progn
    (setq mu4e-compose-complete-only-after nil)
    (setq mu4e-maildir "~/Mail")

    (setq mu4e-drafts-folder "/[Gmail].Drafts")
    (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
    (setq mu4e-trash-folder  "/[Gmail].Trash")

    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
    (setq mu4e-sent-messages-behavior 'delete)

    ;; setup some handy shortcuts
    ;; you can quickly switch to your Inbox -- press ``ji''
    ;; then, when you want archive some messages, move them to
    ;; the 'All Mail' folder by pressing ``

    ;; allow for updating mail using 'U' in the main view:
    (setq mu4e-get-mail-command "offlineimap")
    ;; show images
    (setq mu4e-show-images t)
    (add-hook 'mu4e-compose-mode-hook
              (defun my-do-compose-stuff () (flyspell-mode)))
    (setq mu4e-update-interval (* 60 20))

    ;; ;; something about ourselves
    ;; (setq
    ;;    mu4e-compose-signature
    ;;     (concat
    ;;       "Foo X. Bar\n"
    ;;       "http://www.example.com\n"))

    ;; sending mail -- replace USERNAME with your gmail username
    ;; also, make sure the gnutls command line utils are installed
    ;; package 'gnutls-bin' in Debian/Ubuntu

    (require 'smtpmail)
    ;; (setq message-send-mail-function 'smtpmail-send-it
    ;;    starttls-use-gnutls t
    ;;    smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
    ;;    smtpmail-auth-credentials
    ;;      '(("smtp.gmail.com" 587 "USERNAME@gmail.com" nil))
    ;;    smtpmail-default-smtp-server "smtp.gmail.com"
    ;;    smtpmail-smtp-server "smtp.gmail.com"
    ;;    smtpmail-smtp-service 587)

    ;; alternatively, for emacs-24 you can use:
    (setq message-send-mail-function 'smtpmail-send-it
          smtpmail-stream-type 'starttls
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587)

    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)))
