(defsystem cl-prolog2.gprolog
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "MIT"
  :depends-on (:cl-prolog2)
  :components ((:file "package"))
  :description "CL-PROLOG2 extension for GPROLOG-Prolog"
  :in-order-to ((test-op (test-op :cl-prolog2.gprolog.test)))
  ;; :defsystem-depends-on (:trivial-package-manager)
  ;; :perform
  ;; (load-op :before (op c)
  ;;          (uiop:symbol-call :trivial-package-manager
  ;;                            :ensure-program
  ;;                            "gprolog"
  ;;                            :apt "gprolog"
  ;;                            :yum "gprolog"
  ;;                            :dnf "gprolog"
  ;;                            :pacman "gprolog"
  ;;                            :brew "gnu-prolog"
  ;;                            :macports "gprolog"))
  )
