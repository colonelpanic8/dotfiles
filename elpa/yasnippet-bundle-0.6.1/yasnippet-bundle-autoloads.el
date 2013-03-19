;;; yasnippet-bundle-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (yas/minor-mode yas/root-directory) "yasnippet-bundle"
;;;;;;  "yasnippet-bundle.el" (20808 10390))
;;; Generated autoloads from yasnippet-bundle.el

(defvar yas/root-directory nil "\
Root directory that stores the snippets for each major mode.

If you set this from your .emacs, can also be a list of strings,
for multiple root directories. If you make this a list, the first
element is always the user-created snippets directory. Other
directories are used for bulk reloading of all snippets using
`yas/reload-all'")

(custom-autoload 'yas/root-directory "yasnippet-bundle" nil)

(autoload 'yas/minor-mode "yasnippet-bundle" "\
Toggle YASnippet mode.

When YASnippet mode is enabled, the `tas/trigger-key' key expands
snippets of code depending on the mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

You can customize the key through `yas/trigger-key'.

Key bindings:
\\{yas/minor-mode-map}

\(fn &optional ARG)" t nil)
(require 'yasnippet-bundle)

;;;***

;;;### (autoloads nil nil ("yasnippet-bundle-pkg.el") (20808 10390
;;;;;;  105102))

;;;***

(provide 'yasnippet-bundle-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yasnippet-bundle-autoloads.el ends here
