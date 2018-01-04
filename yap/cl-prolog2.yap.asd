(defsystem cl-prolog2.yap
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "MIT"
  :depends-on (:cl-prolog2)
  :components ((:file "package"))
  :description "CL-PROLOG2 extension for Yap"
  :in-order-to ((test-op (test-op :cl-prolog2.yap.test)))
  :defsystem-depends-on (:trivial-package-manager)
  :perform
  (load-op :before (op c)
           (uiop:symbol-call :trivial-package-manager
                             :ensure-program
                             "yap"
                             :apt "yap"
                             :yum "pl-compat-yap-devel"
                             :dnf "pl-compat-yap-devel"
                             :brew "yap"
                             :env-alist `(("PATH" . ,(format nil "~a:~a"
                                                             (asdf:system-relative-pathname
                                                              :cl-prolog2.yap "yap")
                                                             (uiop:getenv "PATH"))))
                             ;; installs the latest with tabling mode enabled
                             :from-source (format nil "make -C ~a"
                                                  (asdf:system-source-directory :cl-prolog2.yap)))))
