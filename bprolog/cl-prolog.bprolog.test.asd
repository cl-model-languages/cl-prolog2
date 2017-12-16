(defsystem cl-prolog.bprolog.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-prolog.bprolog"
  :license "LLGPL"
  :depends-on (:cl-prolog.bprolog
               :cl-prolog.test)
  :perform (test-op :after (op c)
                    (eval
                     (read-from-string
                      "(let ((cl-prolog.test:*interpreter-class* :bprolog)) (5am:run! :cl-prolog.impl))"))))
