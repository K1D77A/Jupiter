(in-package :jupiter)




(defmacro define-handler (server (method path) lambda)
  (check-type method http-method)
  (check-type path string)
  (alexandria:with-gensyms (handler time)
    `(handler-case
         (let ((,handler (get-handler ,server ,method ,path)))
           (with-accessors ((last-modified last-modified)
                            (response-body-func response-body-func))
               ,handler
             (setf last-modified (time-now))
             (setf response-body-func ,lambda)))
       (no-associated-handler ()
         (let* ((,time (time-now))
                (,handler (make-instance 'handler :response-body-func ,lambda
                                                  :creation-time ,time
                                                  :last-modified ,time)))
           (set-handler ,server ,method ,path ,handler))))))

(defun 404-handler (method url)
  (make-instance 'handler :response-body-func 
                 (lambda () (format t "404 not found~%URL: ~S~%METHOD: ~S~%" method url))
                 :creation-time (time-now)
                 :last-modified (time-now)))



