;;; no-easy-keys.el --- Learn the proper Emacs movement keys

;; Copyright (C) 2009-2012 Dan Amlund Thomsen

;; Author: Dan Amlund Thomsen <dan@danamlund.dk>
;; URL: http://danamlund.dk/emacs/no-easy-keys.html
;; Version: 1.0.2
;; Created: 2009-12-12
;; By: Dan Amlund Thomsen
;; Keywords: training, pinky

;;; Commentary:

;; This mode teaches you to use the proper Emacs movement keys in a
;; rather harsh manner.

;; No-easy-keys disables arrow, end, home and delete keys, as well as
;; their control and meta prefixes. When using any of these keys, you
;; instead get a message informing you of the proper Emacs shortcut
;; you should use instead (e.g. pressing down informs you to use C-n).

;; The easy keys are not disabled in the minibuffer. The minibuffer
;; has different proper keys replacements than regular buffers and
;; depending on various extensions such as icicles, ido, etc.

;; To install, save no-easy-keys.el in your load path and add the
;; following to your .emacs file:
;;
;; (require 'no-easy-keys)
;; (no-easy-keys 1)

;; You can toggle no-easy-keys using 'M-x no-easy-keys'.

;;; Code:

(defvar no-easy-keys-minor-mode-map (make-keymap) 
  "no-easy-keys-minor-mode keymap.")

(let ((f (lambda (m)
           `(lambda () (interactive) 
              (message (concat "No! use " ,m " instead."))))))
  (dolist (l '(("<left>" . "C-b") ("<right>" . "C-f") ("<up>" . "C-p")
               ("<down>" . "C-n")
               ("<C-left>" . "M-b") ("<C-right>" . "M-f") ("<C-up>" . "M-{")
               ("<C-down>" . "M-}")
               ("<M-left>" . "M-b") ("<M-right>" . "M-f") ("<M-up>" . "M-{")
               ("<M-down>" . "M-}")
               ("<delete>" . "C-d") ("<C-delete>" . "M-d")
               ("<M-delete>" . "M-d") ("<next>" . "C-v") ("<C-next>" . "M-x <")
               ("<prior>" . "M-v") ("<C-prior>" . "M-x >") 
               ("<home>" . "C-a") ("<C-home>" . "M->")
               ("<C-home>" . "M-<") ("<end>" . "C-e") ("<C-end>" . "M->")))
    (define-key no-easy-keys-minor-mode-map
      (read-kbd-macro (car l)) (funcall f (cdr l)))))

(define-minor-mode no-easy-keys-minor-mode
  "A minor mode that disables the arrow-keys, pg-up/down, delete
and backspace.

Use 'M-x no-easy-keys' to toggle this mode in all buffers except
the minibuffer.

Add (no-easy-keys 1) to your .emacs to enable no-easy-keys by
default."

nil nil 'no-easy-keys-minor-mode-map)

(defun no-easy-keys-hook ()
  (interactive)
  (unless (minibufferp)
    (no-easy-keys-minor-mode 1)))

(define-globalized-minor-mode no-easy-keys
  no-easy-keys-minor-mode no-easy-keys-hook)

(provide 'no-easy-keys)
;;; no-easy-keys.el ends here
