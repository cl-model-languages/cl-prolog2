(defsystem cl-prolog2.xsb.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-prolog2.xsb"
  :license "LLGPL"
  :depends-on (:cl-prolog2.xsb
               :cl-prolog2.test)
  :perform (test-op :after (op c)
                    (eval
                     (read-from-string
                      "(let ((cl-prolog2.test:*interpreter-class* :xsb)) (5am:run! :cl-prolog2.impl))"))))
