(defsystem cl-prolog2.swi
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "MIT"
  :depends-on (:cl-prolog2)
  :components ((:file "package"))
  :description "CL-PROLOG2 extension for SWI-Prolog"
  :in-order-to ((test-op (test-op :cl-prolog2.swi.test)))
  :defsystem-depends-on (:trivial-package-manager)
  :perform
  (load-op :before (op c)
           (uiop:symbol-call :trivial-package-manager
                             :ensure-program
                             "swipl"
                             :apt "swi-prolog/stable/swi-prolog-nox"
                             :yum "pl"
                             :dnf "pl"
                             :pacman "swi-prolog"
                             :brew "swi-prolog"
                             :choco "swi-prolog")))
