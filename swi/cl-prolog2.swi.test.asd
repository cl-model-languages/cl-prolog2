(defsystem cl-prolog2.swi.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-prolog2.swi"
  :license "MIT"
  :depends-on (:cl-prolog2.swi
               :cl-prolog2.test)
  :perform (test-op :after (op c)
                    (eval
                     (read-from-string
                      "(let ((cl-prolog2.test:*interpreter-class* :swi)) (5am:run! :cl-prolog2.impl))"))))
