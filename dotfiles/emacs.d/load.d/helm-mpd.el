;; (load-file "~/Projects/mingus/libmpdee.el")

;; (setq imalison-mpd-conn (mpd-conn-new "127.0.0.1" 6600))
;; (mpd-search imalison-mpd-conn 'any "nas represent")

;; (defun helm-mpd-search ()
;;   (mapcar 'format-mpd-result-for-helm (mpd-search imalison-mpd-conn 'any helm-pattern)))

;; (defun format-mpd-result-for-helm (mpd-result)
;;   (list (format "%s - %s" (plist-get mpd-result 'Artist) (plist-get mpd-result 'Title))))

;; (mpd-execute-command mpd-inter-conn
;;                      (mapconcat (lambda (song)
;;                                   (format "add %s" (mpd-safe-string song)))
;;                           (split-string song "\n") "\n"))


;; ("Angel in Blue Jeans (3m24s)
;; Train - Angel in Blue Jeans" (track-number . "1") (artists . [((name . "Train") (href . "spotify:artist:3FUY2gzHeIiaesXtOAdB7A"))]) (href . "spotify:track:2OpY5KKUvkYAkiY8FEG1Wi") (length . 204.466) (external-ids . [((id . "USSM11404307") (type . "isrc"))]) (popularity . "0.74") (name . "Angel in Blue Jeans") (album (availability (territories . "AD AR AT AU BE BG BO BR CA CH CL CO CR CY CZ DE DK DO EC EE ES FI FR GB GR GT HK HN HR HU IE IS IT LI LT LU LV MC MT MX MY NI NL NO NZ PA PE PH PL PT PY RO SE SG SI SK SV TR TW US UY")) (name . "Angel in Blue Jeans") (href . "spotify:album:00QRaEtUaplb5qxN62o9vd") (released . "2014")))



;; (plist-get '(file "spotify:album:0xvDtkNKJiLclVbjLvovFU" Time 0 Artist "Angel Olsen" Title "Album: Burn Your Fire For No Witness" Album "Burn Your Fire For No Witness" Date "2014" Track "0" AlbumArtist "Angel Olsen") 'file)

;; (defvar helm-source-mpd-search
;;   '((name . "Spotify")
;;     (volatile)
;;     (requires-pattern . 2)
;;     (candidates-process . helm-mpd-search)))

;; (helm-mpd-search)

;; (helm :sources '(helm-source-mpd-search)
;;       :buffer "*helm-mpd*")

;; (let ((helm-pattern "nas"))
;;   (helm-mpd-search))

;; (mingus-make-status-string)
