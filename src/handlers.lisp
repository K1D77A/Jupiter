(in-package :jupiter)

(defun 404-handler (method url)
  (format t "404 not found~%URL: ~S~%METHOD: ~S~%" method url)
  (force-output t))

(defmacro define-handler (server (method path &body body))
  (check-type method http-method)
  (check-type path string)
  `(set-handler ,server ,method ,path
                (lambda (packet)
                  ,@body)))


