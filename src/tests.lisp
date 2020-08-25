(in-package :jupiter)

(defparameter *server* (make-server :port 9001))


(define-handler *server* (:GET "/greetings")
                (lambda (request response)
                  (declare (ignore request response))
                  (princ "kaboof")
                  (princ "doof")
                  (princ "loof")))
