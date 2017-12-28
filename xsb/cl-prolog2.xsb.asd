(defsystem cl-prolog2.xsb
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:cl-prolog2)
  :components ((:file "package"))
  :description "CL-PROLOG2 extension for XSB, a research-oriented, commercial-grade Logic Programming system for Unix
and Windows-based platforms."
  :in-order-to ((test-op (test-op :cl-prolog2.xsb.test)))
  :defsystem-depends-on (:trivial-package-manager)
  :perform
  (load-op :before (op c)
           (uiop:symbol-call :trivial-package-manager
                             :ensure-program
                             "xsb"
                             :env-alist `(("PATH" . ,(format nil "~a:~a"
                                                             (asdf:system-relative-pathname
                                                              :cl-prolog2.xsb "XSB/bin")
                                                             (uiop:getenv "PATH"))))
                             :from-source (format nil "make -C ~a"
                                                  (asdf:system-source-directory :cl-prolog2.xsb)))))
