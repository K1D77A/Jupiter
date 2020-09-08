;;;; jupiter.asd

(asdf:defsystem #:jupiter
  :description "A trivial HTTP server"
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :depends-on (#:str
               #:usocket
               #:bordeaux-threads
               #:metalock
               #:alexandria
               #:cl-mime-from-string
               #:trivial-backtrace
               #:percent-encoding
               #:log4cl
               #:closer-mop
               #:cacle
               #:local-time)
  :components ((:file "package")
               (:file "helpers")
               (:file "classes")
               (:file "requests")
               (:file "response")
               (:file "handlers")
               (:file "server")
               (:file "tests")))
