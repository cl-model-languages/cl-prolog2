(defsystem cl-prolog2.gprolog.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-prolog2.gprolog"
  :license "MIT"
  :depends-on (:cl-prolog2.gprolog
               :cl-prolog2.test)
  :perform (test-op :after (op c)
                    #+(or)
                    (eval
                     (read-from-string
                      "(let ((cl-prolog2.test:*interpreter-class* :gprolog-interpreted)) (5am:run! :cl-prolog2.impl))"))
                    (eval
                     (read-from-string
                      "(let ((cl-prolog2.test:*interpreter-class* :gprolog)) (5am:run! :cl-prolog2.impl))"))))
