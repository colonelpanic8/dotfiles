;; -*- lexical-binding: t; -*-

;; Emacs 31 reports missing lexical-binding cookies in third-party packages
;; that are managed outside this checkout.  Project-owned files carry cookies.
(defvar warning-suppress-log-types nil)
(add-to-list 'warning-suppress-log-types '(files missing-lexbind-cookie))

(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
