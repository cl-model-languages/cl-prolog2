(defsystem cl-prolog.swi.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-prolog.swi"
  :license "MIT"
  :depends-on (:cl-prolog.swi
               :cl-prolog.test)
  :perform (test-op :after (op c)
                    (eval
                     (read-from-string
                      "(let ((cl-prolog.test:*interpreter-class* :swi)) (5am:run! :cl-prolog.impl))"))))
