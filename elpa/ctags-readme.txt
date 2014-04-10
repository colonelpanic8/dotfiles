Configuration example:

(setq tags-revert-without-query t)
(global-set-key (kbd "<f7>") 'ctags-create-or-update-tags-table)

Then just press <f7> to update or create your TAGS file. That function look
for a file TAGS in the current and its parent directories, if a TAG file is
not found it ask you where create a new one.

Optionally you can use the function `ctags-search', a little wrapper for
`tags-search' that provides a default input like in `find-tag', to search
through all files listed in tags table.

Also, if you prefer, you can override the key binding M-. for `find-tag' to
use `ctags-search':

(global-set-key (kbd "M-.")  'ctags-search)

Installation:

Add this to your Emacs configuration:

(add-to-list 'load-path "/folder/containing/file")
(require 'ctags)

Alternatively, you can install it using the Marmalade ELPA repository.
