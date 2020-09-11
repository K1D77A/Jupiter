;;;; jupiter.asd

(asdf:defsystem #:jupiter
  :description "A trivial HTTP server"
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.2"
  :serial nil
  :pathname "src/"
  :depends-on (#:str
               #:usocket
               #:bordeaux-threads
               #:queues.simple-cqueue
               #:metalock
               #:alexandria
               #:cl-mime-from-string
               #:trivial-backtrace
               #:percent-encoding
               #:log4cl
               #:lisp-unit
               #:fast-io
               #:closer-mop
               #:cacle
               #:local-time)
  :components ((:file "package")
               (:file "helpers")
               (:file "requests")
               (:file "cookies")
               (:file "server")
               (:file "threads")
               (:file "response")               
               (:file "handlers")
               (:file "tests/tests")))
