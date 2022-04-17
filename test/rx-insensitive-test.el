;;; rx-insensitive-test
(require 'rx-insensitive)

(defconst rx-insensitive--tests
  `((:in (?a) :out ((or ?a ?A)))
    (:in ((or ?a ?c)) :out ((or (or ?a ?A)
                                (or ?c ?C))))
    (:in (lower) :out (alpha))
    (:in (upper) :out (alpha))
    ;; apparently (rx alpha) != (rx (or alpha alpha))
    (:in ((or lower upper)) :out ((or alpha alpha)))
    (:in ((any ?a "c-e")) :out ((any "ac-eAC-E")))
    (:in (string-start) :out (string-start))
    (:in (string-end) :out (string-end))
    )
  "Test Suites defined as a list of plists, whose :in value is a
list which is fed to `rx-insensitive', and its conversion should
be equivalent to the :out value of the plist.")

(defun rx-insensitive--run-tests ()
  (require 'cl-lib)
  (dolist (test rx-insensitive--tests)
    (let* ((in (plist-get test :in))
           (out (plist-get test :out))
           (insensitive-expr `(rx-insensitive
                               . ,(rx-insensitive--each in)))
           (insensitive (eval insensitive-expr))
           (sensitive-expr  `(rx . ,out))
           (sensitive (eval sensitive-expr)))
      (cl-assert (string-equal insensitive sensitive)
                 :show-args
                 "Values not the same: %s and %s"
                 insensitive-expr sensitive-expr))))

(rx-insensitive--run-tests)
