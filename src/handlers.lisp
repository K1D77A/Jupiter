(in-package :jupiter)



;;;this is another dead end, I think a better idea is to use (define-handler ..) to expand calls to
;;;certain functions like '(age request)' to (second (assoc 'age (headers request) :test #'eq)) etc
;;;and expand a (setf (age request) "zonk") to
;;;(setf (second (assoc 'age (headers request) :test #'eq)) "zonk")
;;;and do that by storing an association between certain symbols and expansions. could be fun



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

(defun timeout-handler ()
  (make-instance 'handler
                 :response-body-func
                 (lambda () (format t "504 connection timed out"))
                 :creation-time (time-now)
                 :last-modified (time-now)))



