;;; ctags-update.el --- (auto) update TAGS in parent directory using exuberant-ctags

;; Created: 2011-10-16 13:17
;; Last Updated: 纪秀峰 2013-11-07 17:45:50 4
;; Version: 20131125.743
;; X-Original-Version: 0.2.2
;; Author: Joseph(纪秀峰)  jixiuf@gmail.com
;; Keywords: exuberant-ctags etags
;; URL: https://github.com/jixiuf/helm-etags-plus
;;      https://github.com/emacsmirror/ctags-update

;; Copyright (C) 2011,2012 Joseph(纪秀峰) all rights reserved.

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

;; Just put ctags-update.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;;(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
;;(add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
;; ...
;;(add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode)
;;
;; then when you save a file ,`ctags-auto-update-mode' will recursively searches each
;; parent directory for a file named 'TAGS'. if found ,it will use
;; `exuberant-ctags' update TAGS,
;; it would not be updated if last time calling `ctags-update' is not 5 minute age(default).
;; if no 'TAGS' found ,it will check `tags-table-list' and `tags-file-name'
;; if current buffer shares the same parent directory with `tags-file-name' or one element of
;; `tags-table-list' , it will auto create 'TAGS' file
;; eq:
;;    (setq tags-file-name "/tmp/TAGS")
;;  or
;;    (setq tags-table-list '("/tmp/TAGS"))
;;
;; if you want to update (create) TAGS manually
;; you can
;;     (autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)
;;     (global-set-key "\C-cE" 'ctags-update)
;; with prefix `C-u' ,then you can generate a new TAGS file in your selected directory,
;; with prefix `C-uC-u' same to prefix `C-u',but save it to kill-ring instead of execute it."

;;
;; on windows ,you can custom `ctags-update-command' like this:
;; (when (equal system-type 'windows-nt)
;;   (setq ctags-update-command (expand-file-name  "~/.emacs.d/bin/ctags.exe")))

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `ctags-update'
;;    update TAGS in parent directory using `exuberant-ctags'.
;;  `ctags-auto-update-mode'
;;    auto update TAGS using `exuberant-ctags' in parent directory.
;;  `turn-on-ctags-auto-update-mode'
;;    turn on `ctags-auto-update-mode'.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `ctags-update-command'
;;    it only support `exuberant-ctags'
;;    default = "ctags"
;;  `ctags-update-delay-seconds'
;;    in `after-save-hook' current-time - last-time must bigger than this value,
;;    default = (* 5 60)
;;  `ctags-update-other-options'
;;    other options for ctags
;;    default = (list "--exclude='*.elc'" "--exclude='*.class'" "--exclude='.git'" "--exclude='.svn'" ...)
;;  `ctags-update-lighter'
;;    Lighter displayed in mode line when `ctags-auto-update-mode'
;;    default = " ctagsU"

;;; Code:

(require 'etags)

(defgroup ctags-update nil
  "auto update TAGS in parent directory using `exuberant-ctags'"
  :prefix "ctags-update"
  :group 'etags)

(defcustom ctags-update-command "ctags"
  "it only support `exuberant-ctags'
take care it is not the ctags in `emacs-VERSION/bin/'
you should download `exuberant-ctags' and make sure
the ctags is under $PATH before `emacs-VERSION/bin/'"
  :type 'string
  :group 'ctags-update)

(defcustom ctags-update-delay-seconds  (* 5 60) ; 5 mins
"in `after-save-hook' current-time - last-time must bigger than this value,
then `ctags-update' will be called"
  :type 'integer
  :group 'ctags-update)

(defcustom ctags-update-other-options
  (list
   "--fields=+iaSt"
   "--extra=+q"
   "−−c++−kinds=+p"
   "--exclude='*.elc'"
   "--exclude='*.class'"
   "--exclude='.git'"
   "--exclude='.svn'"
   "--exclude='SCCS'"
   "--exclude='RCS'"
   "--exclude='CVS'"
   "--exclude='EIFGEN'")
  "other options for ctags"
  :group 'ctags-update
  :type '(repeat string))

(defcustom ctags-update-lighter " ctagsU"
  "Lighter displayed in mode line when `ctags-auto-update-mode'
is enabled."
  :group 'ctags-update
  :type 'string)

(defvar ctags-update-last-update-time
  (- (float-time (current-time)) ctags-update-delay-seconds 1)
  "make sure when user first call `ctags-update' it can run immediately")

(defvar ctags-auto-update-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(defvar  ctags-auto-update-mode-hook nil)

(defvar ctags-update-use-xemacs-etags-p
  (fboundp 'get-tag-table-buffer)
  "Use XEmacs etags?")

(defun ctags-update-file-truename (filename) "empty function")

(if ctags-update-use-xemacs-etags-p
    (unless (fboundp 'symlink-expand-file-name)
      (fset 'symlink-expand-file-name 'file-truename)))
(if (fboundp 'symlink-expand-file-name)
    (fset 'ctags-update-file-truename 'symlink-expand-file-name)
  (fset 'ctags-update-file-truename 'file-truename))

(defun ctags-update-command-args (tagfile-full-path &optional save-tagfile-to-as)
  "`tagfile-full-path' is the full path of TAGS file . when files in or under the same directory
with `tagfile-full-path' changed ,then TAGS file need to be updated. this function will generate
the command to update TAGS"
  ;; on windows "ctags -R d:/.emacs.d"  works , but "ctags -R d:/.emacs.d/" doesn't
  (let*( (tagdir-with-slash-appended (expand-file-name (file-name-directory tagfile-full-path)))
         (length-of-tagfile-directory (length tagdir-with-slash-appended))
         (tagdir-without-slash-appended (substring tagdir-with-slash-appended 0 (1- length-of-tagfile-directory)))
         (args
          (append
           (list "-R" "-e" )
           (when (equal system-type 'windows-nt)
             (list "-f" (get-system-file-path (or save-tagfile-to-as tagfile-full-path))))
           ctags-update-other-options
           (if (equal system-type 'windows-nt)
               (list tagdir-without-slash-appended)
             (list "."))
           )))
    args))
(defun ctags-update-get-command(command command-args)
  "get the full command as string."
  (concat command " "(mapconcat 'identity  command-args " ")))


(defun get-system-file-path(file-path)
  "when on windows `expand-file-name' will translate from \\ to /
some times it is not needed . then this function is used to translate /
to \\ when on windows"
  (if (equal system-type 'windows-nt)
      (convert-standard-filename  file-path)
    file-path))

(defun ctags-update-find-tags-file ()
  "recursively searches each parent directory for a file named 'TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
  (let ((tag-root-dir (locate-dominating-file default-directory "TAGS")))
    (if tag-root-dir
        (expand-file-name "TAGS" tag-root-dir)
      (if (and tags-file-name
               (string-match (regexp-quote (ctags-update-file-truename  default-directory))
                             (ctags-update-file-truename tags-file-name)))
          tags-file-name
        (if tags-table-list
            (let (matched-tag-names match-tag-element)
              (dolist (tagname tags-table-list)
                (when (string-match (regexp-quote (ctags-update-file-truename  default-directory))
                                    (ctags-update-file-truename tagname))
                  (add-to-list 'matched-tag-names tagname)))
              (when matched-tag-names
                (setq match-tag-element (car matched-tag-names))
                      (if (file-directory-p match-tag-element)
                          (expand-file-name "TAGS"  match-tag-element)
                        match-tag-element))))))))

;;;###autoload
(defun ctags-update(&optional args)
  "update TAGS in parent directory using `exuberant-ctags'.
1. you can call this function directly,
2. enable `ctags-auto-update-mode',
3. with prefix `C-u' then you can generate a new TAGS file in directory,
4. with prefix `C-uC-u' save the command to kill-ring instead of execute it."
  (interactive "P")
  (let (tags-filename process)
    (when (or (and args (setq tags-filename
                              (expand-file-name
                               "TAGS" (read-directory-name "Generate new TAGS to directory:" ))))
              (and (not (get-process "update TAGS"));;if "update TAGS" process is not already running
                   (or (called-interactively-p 'interactive)
                       (> (- (float-time (current-time))
                             ctags-update-last-update-time)
                          ctags-update-delay-seconds))
                   (setq tags-filename (ctags-update-find-tags-file))
                   (not (and (buffer-file-name)
                             (string-equal (ctags-update-file-truename tags-filename)
                                           (ctags-update-file-truename (buffer-file-name)))
                             ))))
      (setq ctags-update-last-update-time (float-time (current-time)));;update time
      (let ((orig-default-directory default-directory)
            (default-directory (file-name-directory tags-filename)))
        (when (equal system-type 'windows-nt)
          (setq default-directory orig-default-directory))
        (cond
         ;;with prefix `C-uC-u' save the command to kill-ring
         ;; sometime the directory you select need root privilege
         ;; so save the command to kill-ring,
         ((and (called-interactively-p 'interactive) args (equal args '(16)))
          (kill-new (format "cd %s && %s" default-directory
                            (ctags-update-get-command
                             ctags-update-command (ctags-update-command-args tags-filename))))
          (message "save ctags-upate command to king-ring. (C-y) yank it back."))
         (t
          (setq process
                (apply 'start-process ;;
                       "update TAGS" " *update TAGS*"
                       ctags-update-command
                       (ctags-update-command-args tags-filename)))
          (set-process-sentinel process
                                (lambda (proc change)
                                  (when (string-match "\\(finished\\|exited\\)" change)
                                    (kill-buffer " *update TAGS*")
                                    (message "TAGS in parent directory is updated. "  )
                                    )))
          ))))))

;;;###autoload
(define-minor-mode ctags-auto-update-mode
  "auto update TAGS using `exuberant-ctags' in parent directory."
  :lighter ctags-update-lighter
  :keymap ctags-auto-update-mode-map
  ;; :global t
  :init-value nil
  :group 'ctags-update
  (if ctags-auto-update-mode
      (progn
        (add-hook 'after-save-hook 'ctags-update nil t)
        (run-hooks 'ctags-auto-update-mode-hook))
    (remove-hook 'after-save-hook 'ctags-update t)))

;;;###autoload
(defun turn-on-ctags-auto-update-mode()
  "turn on `ctags-auto-update-mode'."
  (interactive)
  (ctags-auto-update-mode 1))

(provide 'ctags-update)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:

;;; ctags-update.el ends here
