(defsystem cl-prolog.swi
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "MIT"
  :depends-on (:cl-prolog)
  :components ((:file "package"))
  :description "CL-PROLOG extension for SWI-Prolog"
  :in-order-to ((test-op (test-op :cl-prolog.swi.test)))
  :defsystem-depends-on (:trivial-package-manager)
  :perform
  (load-op :before (op c)
           (uiop:symbol-call :trivial-package-manager
                             :ensure-program
                             "swipl"
                             :apt "swi-prolog/stable/swi-prolog"
                             :yum "pl"
                             :dnf "pl"
                             :pacman "swi-prolog"
                             :brew "swi-prolog"
                             :choco "swi-prolog")))
