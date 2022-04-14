;;; rx-insensitive.el --- rx but case-insensitive -*- lexical-binding: t; -*-

;;; Commentary:

;;; This package takes the power of rx, with case-insensitive
;;; enabled.


;;; Code:

;;; Regular rx:

;;; (rx string-start "hello-world" string-end)
;;; -> "\\`hello-world\\'"

;;; (rx-insensitive string-start "hello-world" string-end)
;;;-> "\\`[Hh][Ee][Ll][Ll][Oo]-[Ww][Oo][Rr][Ll][Dd]\\'"
(require 'rx)

(defun rx-insensitive--char-insensitize (char)
  "Convert a character CHAR for case-insensitive matching.
Return a list of characters that should match CHAR
case-insensitively.  See `capitalize' and `downcase'."
  (when (characterp char)
     `(,(capitalize char) ,(downcase char))))

(defun rx-insensitive--range-separate-step
    (range-start range-end seps &optional acc)
  "This is a helper function for `rx-insensitive--range-separate'.
For RANGE-START and RANGE-END see
`rx-insensitive--range-separate'.

SEPS should be a strictly-increasing list.  Each element of SEPS,
SEP, marks the place to break up the range (inclusive with the
lower-valued range).  That is, with the range `(RANGE-START
. RANGE-END)', break it up to `(RANGE-START . SEP)' and `((1+
SEP) . RANGE-END)' if necessary.

ACC is internal use only, for
accumulating list of ranges."
  (let ((sep (car seps)))
    (cond
     ((> range-start range-end) acc)
     ((not seps) acc)
     ;; if start >= sep, next sep
     ((>= range-start sep)
      (rx-insensitive--range-separate-step
       range-start range-end (cdr seps) acc))
     ;; if start < sep, then break it
     (t (rx-insensitive--range-separate-step
         (1+ sep) range-end (cdr seps)
         `((,range-start . ,(min sep range-end)) . ,acc))
        ))))

(defconst rx-insensitive--separate-steps
  `(,(1- ?A) ?Z                         ; upper case
    ,(1- ?a) ?z                         ; lower case
    )
"The list of separation steps used as an initial value of
`rx-intensive--range-separate-step'.")

(defun rx-insensitive--range-insensitize (range)
  "Make RANGE case-insensitive.  RANGE should be either a two
element list `(RANGE-START RANGE-END)' or a `cons'
pair `(RANGE-START . RANGE-END)'.  Return a list of `cons'
ranges.  The input range should have already been separated."
  (let* ((range-start (car range))
         (range-end (cdr range))
         (range-end (or (car-safe range-end) range-end)))
    `((,(downcase range-start) . ,(downcase range-end))
      (,(capitalize range-start) . ,(capitalize range-end)))))

(defun rx-insensitive--range-separate (range-start range-end)
  "Try to separate the range `(RANGE-START . RANGE-END)'.  Return a
list of `cons' cells as ranges.  The goal is that when the input
range crosses character range, separate it so that the new ranges
do not cross character ranges.  Note that I have only concerned
about latin characters but it is easily-extensible to larger
ranges.  When the range is empty, return nil."
  (when (and (characterp range-start)
             (characterp range-end))
    (rx-insensitive--range-separate-step
     range-start range-end rx-insensitive--separate-steps)))

(defun rx-insensitive--convert-string-range
    (range-str &optional prev-char acc)
  "Convert RANGE-STR into list of `cons' ranges or characters to
feed into the any form of `rx'.  PREV-CHAR is internal use for
the previous character.  ACC is internal use for accumulated
results.  The resultant ranges have been passed to
`rx-insensitive--range-separate' when returned."
  (let ((new-acc (if prev-char `(,prev-char . ,acc) acc)))
    (cond
     ;; no more input, stop
     ((length= range-str 0) new-acc)
     ;; have prev + start with dash + have "range-end" = range
     ((and (length> range-str 1)
           (= ?- (aref range-str 0))
           prev-char)
      (let* ((range-end (aref range-str 1))
             (ins-ranges (rx-insensitive--range-separate
                          prev-char range-end))
             (acc (append ins-ranges acc)))
        (rx-insensitive--convert-string-range
         (substring range-str 2) nil acc)))
     ;; any char, push to prev (use as character)
     (t (rx-insensitive--convert-string-range
         (substring range-str 1) (aref range-str 0) new-acc)))))

(defun rx-insensitive--process-any (any-arg)
  "Process a single argument, ANY-ARG, from the any form for
`rx-insensitive'.  Return a list of arguments to be fed to the
any form of `rx'."
  (pcase any-arg
    ;; string -> cons ranges and characters
    ((pred stringp)
     (mapcan #'rx-insensitive--process-any
             (rx-insensitive--convert-string-range any-arg)))
    ;; cons range, try to separate
    ((and `(,(pred characterp) . ,(pred characterp))
          `(,start . ,end))
     (mapcan #'rx-insensitive--range-insensitize
             (rx-insensitive--range-separate start end)))
    ;; single character
    ((pred characterp) (rx-insensitive--char-insensitize any-arg))
    (_ any-arg)))

(defun rx-insensitive--each (arg)
  "Process a single item ARG from argument list of `rx-insensitive'.
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
    (`(,(or 'any 'in 'char) . ,rest)
     `(any ,@(mapcan #'rx-insensitive--process-any rest)))
    ;; for a list, convert each argument
    ((and (pred listp) list)
     (mapcar #'rx-insensitive--each list))
    ;; other values are error
    ))

;;;###autoload
(defmacro rx-insensitive (&rest args)
  "Create a case-insensitive regular expression similar to `rx'
with a list of arguments ARGS.

A warning on using ranges in the any form in `rx-insensitive': be
very careful with ranges *crossing* characters, which may have
incorrect behaviors."
  `(rx ,@(mapcar #'rx-insensitive--each args)))

(provide 'rx-insensitive)
;;; rx-insensitive.el ends here
