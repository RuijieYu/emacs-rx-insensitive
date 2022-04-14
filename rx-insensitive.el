;;; rx-insensitive.el -*- lexical-binding: t; -*-
(require 'rx)

(defun rx-insensitive--char-insensitize (char)
  "Convert a character CHAR for case-insensitive matching.
Return a list of characters that should match CHAR
case-insensitively.  See `capitalize' and `downcase'."
  (when (characterp char)
     `(,(capitalize char) ,(downcase char))))

(defun rx-insensitive--process-any (any-arg)
  "Process a single argument of the *any* form for
`rx-insensitive'.  Return a list of arguments to be fed to *any*
form of `rx'."
  (pcase any-arg
    ((pred stringp) `(,(capitalize any-arg) ,(downcase any-args)))
    ((and `(,(pred characterp) . ,(pred characterp))
          `(,front . ,back))
     (rx-insensitive--process-any (string front ?- back)))
    (_ any-arg)))

(defun rx-insensitive--each (arg)
  "Process a single item from argument list of `rx-insensitive'.
Return arguments to be supplied to `rx'."
  (pcase arg
    ;; when character, convert to upper and lower
    ((pred characterp)
     `(or ,@(rx-insensitive--char-insensitize arg)))
    ;; when string, each character is processed separately
    ((pred stringp)
     `(seq ,@(mapcar #'rx-insensitive--each arg)))
    ;; when case-sensitive char class, convert to insensitive; in
    ;; my understanding, (or lower upper) <=> alpha
    ((or 'lower 'lower-case 'upper 'upper-case) 'alpha)
    ;; what is semivowel-lower? I don't think there is a
    ;; corresponding uppercase counterpart, and it does not match
    ;; with "a...z", so I'm not worried about it
    ('semivowel arg)
    ;; (eval EXPR): evaluate EXPR and convert insensitive
    (`(eval . ,expr)
     (let ((expr (car expr)))
       (rx-insensitive--each (eval expr))))
    ;; when symbol, pass as-is
    ((pred symbolp) arg)
    ;; process "any" form. This is problematic when a range
    ;; *includes* characters.  For now, naïvely "and" the
    ;; lower-case and the upper-case.
    (`(any . ,rest)
     `(any ,@(mapcan #'rx-insensitive--process-any rest)))
    ;; for a list, convert each argument
    ((and (pred listp) list)
     (mapcar #'rx-insensitive--each list))
    ;; other values are error
    ))

(defmacro rx-insensitive (&rest args)
  "Create a case-insensitive regular expression similar to `rx'."
  `(rx ,@(mapcar #'rx-insensitive--each args)))
