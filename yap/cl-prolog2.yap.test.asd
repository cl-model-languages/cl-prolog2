(defsystem cl-prolog2.yap.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-prolog2.yap"
  :license "MIT"
  :depends-on (:cl-prolog2.yap
               :cl-prolog2.test)
  :perform (test-op :after (op c)
                    (eval
                     (read-from-string
                      "(let ((cl-prolog2.test:*interpreter-class* :yap)) (5am:run! :cl-prolog2.impl))"))))
