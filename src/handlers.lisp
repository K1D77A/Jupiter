(in-package :jupiter)

(defun time-now ()
  (let ((timezone (butlast local-time:+rfc-1123-format+)))
    (format nil "~A~A" (local-time:format-timestring nil (local-time:now)
                                                     :format timezone 
                                                     :timezone local-time:+utc-zone+)
            "GMT")))


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
  (format t "404 not found~%URL: ~S~%METHOD: ~S~%" method url)
  (force-output t))



