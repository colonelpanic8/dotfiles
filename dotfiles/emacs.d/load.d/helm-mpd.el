;; (load-file "~/Projects/libmpdee/libmpdee.el")

;; (setq imalison-mpd-conn (mpd-conn-new "10.0.0.137" 6600))
;; (mpd-search imalison-mpd-conn 'any "nas represent")

;; (defun helm-mpd-search ()
;;   (mapcar 'format-mpd-result-for-helm (mpd-search imalison-mpd-conn 'any helm-pattern)))

;; (defun format-mpd-result-for-helm (mpd-result)
;;   (list (format "%s - %s" (plist-get mpd-result 'Artist) (plist-get mpd-result 'Title))))

;; (defvar helm-source-mpd-search
;;   '((name . "Spotify")
;;     (volatile)
;;     (requires-pattern . 2)
;;     (candidates-process . helm-mpd-search)))

;; ;; (helm-mpd-search)

;; ;; (helm :sources '(helm-source-mpd-search)
;; ;;       :buffer "*helm-mpd*")
