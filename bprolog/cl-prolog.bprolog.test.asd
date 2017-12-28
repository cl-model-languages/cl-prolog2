(defsystem cl-prolog2.bprolog.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-prolog2.bprolog"
  :license "LLGPL"
  :depends-on (:cl-prolog2.bprolog
               :cl-prolog2.test)
  :perform (test-op :after (op c)
                    (eval
                     (read-from-string
                      "(let ((cl-prolog2.test:*interpreter-class* :bprolog)) (5am:run! :cl-prolog2.impl))"))))
