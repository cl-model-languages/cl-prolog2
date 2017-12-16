(defsystem cl-prolog.yap.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-prolog.yap"
  :license "MIT"
  :depends-on (:cl-prolog.swi
               :cl-prolog.test)
  :perform (test-op :after (op c)
                    (eval
                     (read-from-string
                      "(let ((cl-prolog.test:*interpreter-class* :yap)) (5am:run! :cl-prolog.impl))"))))
