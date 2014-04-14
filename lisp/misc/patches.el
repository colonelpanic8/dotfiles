(if (equal (cons 1 1)
             (ignore-errors
               (subr-arity (symbol-function 'execute-extended-command))))
(progn (message "Patching execute-extended-command")
(defun execute-extended-command (prefixarg &optional command-name)
  ;; Based on Fexecute_extended_command in keyboard.c of Emacs.
  ;; Aaron S. Hawley <aaron.s.hawley(at)gmail.com> 2009-08-24
  "Read a command name, then read the arguments and call the command.
Interactively, to pass a prefix argument to the command you are
invoking, give a prefix argument to `execute-extended-command'.
Noninteractively, the argument PREFIXARG is the prefix argument to
give to the command you invoke."
  (interactive (list current-prefix-arg (read-extended-command)))
  ;; Emacs<24 calling-convention was with a single `prefixarg' argument.
  (if (null command-name)
      (setq command-name (let ((current-prefix-arg prefixarg)) ; for prompt
                           (read-extended-command))))
  (let* ((function (and (stringp command-name) (intern-soft command-name)))
         (binding (and suggest-key-bindings
		       (not executing-kbd-macro)
		       (where-is-internal function overriding-local-map t))))
    (unless (commandp function)
      (error "`%s' is not a valid command name" command-name))
    (setq this-command function)
    ;; Normally `real-this-command' should never be changed, but here we really
    ;; want to pretend that M-x <cmd> RET is nothing more than a "key
    ;; binding" for <cmd>, so the command the user really wanted to run is
    ;; `function' and not `execute-extended-command'.  The difference is
    ;; visible in cases such as M-x <cmd> RET and then C-x z (bug#11506).
    (setq real-this-command function)
    (let ((prefix-arg prefixarg))
      (command-execute function 'record))
    ;; If enabled, show which key runs this command.
    (when binding
      ;; But first wait, and skip the message if there is input.
      (let* ((waited
              ;; If this command displayed something in the echo area;
              ;; wait a few seconds, then display our suggestion message.
              (sit-for (cond
                        ((zerop (length (current-message))) 0)
                        ((numberp suggest-key-bindings) suggest-key-bindings)
                        (t 2)))))
        (when (and waited (not (consp unread-command-events)))
          (with-temp-message
              (format "You can run the command `%s' with %s"
                      function (key-description binding))
            (sit-for (if (numberp suggest-key-bindings)
                         suggest-key-bindings
                       2))))))))) (message "Not patching execute-extended-command"))

(provide 'patches)
